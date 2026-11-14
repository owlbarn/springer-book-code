"""Replay recorded actions in GRAY8 and RGB24; export real colour frames.
Run in the recorded book environment, in the directory holding *-actions.csv
and the four original *.pgm frames produced by record_actions.ml.
"""
import csv, hashlib, json
from pathlib import Path
import numpy as np
from PIL import Image
import vizdoom as vzd

checks = []
for label in ('before', 'after'):
    with open(label + '-actions.csv') as f:
        actions = list(csv.DictReader(f))
    for colour in (False, True):
        game = vzd.DoomGame()
        game.load_config(str(Path(vzd.scenarios_path) / 'basic.cfg'))
        game.set_window_visible(False)
        game.set_screen_format(vzd.ScreenFormat.RGB24 if colour else vzd.ScreenFormat.GRAY8)
        game.set_screen_resolution(vzd.ScreenResolution.RES_160X120)
        game.set_seed(1000)
        game.init()
        game.new_episode()
        initial = game.get_state().screen_buffer.copy()
        last = initial
        timeout = game.get_episode_timeout()
        limit = game.get_episode_start_time() + timeout
        for row in actions:
            buttons = [0] * len(game.get_available_buttons())
            buttons[int(row['action'])] = 1
            reward = game.make_action(buttons, 4)
            ended = game.is_episode_finished()
            truncated = ended and timeout > 0 and game.get_episode_time() >= limit
            assert reward == float(row['reward'])
            assert (ended and not truncated) == (row['terminated'] == 'true')
            assert truncated == (row['truncated'] == 'true')
            state = game.get_state()
            if state is not None:
                last = state.screen_buffer.copy()
        assert game.is_episode_finished()
        for suffix, pixels in (('0000', initial), ('final', last)):
            if colour:
                assert pixels.shape == (120, 160, 3)
                assert np.any(pixels[:, :, 0] != pixels[:, :, 1])
                Path(f'{label}-{suffix}.ppm').write_bytes(
                    b'P6\n160 120\n255\n' + pixels.tobytes())
            else:
                assert np.array_equal(pixels, np.array(Image.open(f'{label}-{suffix}.pgm')))
        checks.append({'policy': label, 'format': 'RGB24' if colour else 'GRAY8',
                       'actions': len(actions), 'all_rewards_and_boundaries_match': True,
                       'original_gray_frames_match': not colour})
        game.close()
Path('replay-checks.json').write_text(json.dumps(checks, indent=2) + '\n')
print('PASS: both policies replay identically in GRAY8 and RGB24; four real colour frames exported.')
