#!/usr/bin/env python3
"""Delete superseded build-* tags on Docker Hub after a successful publication.

Kept: latest, the build tag that latest points to, and every tag that does not
start with build- (for example an edition tag). The book records source
commits, so older images are not needed. A token without delete permission
only produces a warning: publication has already succeeded.
"""
import json
import os
import urllib.error
import urllib.request

NAMESPACE, NAME = 'matrixanger', 'springer-book-code'
HUB = 'https://hub.docker.com/v2'


def request(url, method='GET', data=None, token=None):
    headers = {'Content-Type': 'application/json'}
    if token:headers['Authorization'] = 'Bearer '+token
    body = json.dumps(data).encode() if data is not None else None
    with urllib.request.urlopen(urllib.request.Request(url, body, headers, method=method), timeout=60) as response:
        text = response.read().decode()
    return json.loads(text) if text else None


def tags():
    url, found = f'{HUB}/repositories/{NAMESPACE}/{NAME}/tags?page_size=100', {}
    while url:
        page = request(url)
        found.update({tag['name']:tag.get('digest') for tag in page['results']})
        url = page.get('next')
    return found


def superseded(found):
    latest = found.get('latest')
    if not latest:raise SystemExit('No latest tag; nothing is pruned.')
    return sorted(name for name, digest in found.items() if name.startswith('build-') and digest != latest)


def main():
    stale = superseded(tags())
    if not stale:
        print('No superseded build tags.')
        return
    secret = os.environ['HUB_TOKEN']
    try:
        try:
            token = request(f'{HUB}/auth/token', 'POST', {'identifier':NAMESPACE, 'secret':secret})['access_token']
        except urllib.error.HTTPError:  # older login endpoint
            token = request(f'{HUB}/users/login', 'POST', {'username':NAMESPACE, 'password':secret})['token']
        for name in stale:
            request(f'{HUB}/repositories/{NAMESPACE}/{NAME}/tags/{name}/', 'DELETE', token=token)
            print('Deleted', name)
    except urllib.error.HTTPError as error:
        print(f'::warning::Docker Hub returned {error.code} while deleting old build tags; '
              'the DOCKERHUB_TOKEN needs Read, Write & Delete access. Publication is unaffected.')


if __name__ == '__main__':main()
