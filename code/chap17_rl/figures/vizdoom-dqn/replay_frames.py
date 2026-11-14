"""Replay the recorded evaluation actions in RGB24 and save every simulator tic.

Run in the book environment, in this directory: python replay_frames.py OUT
Each recorded action is repeated for four tics, as in training. Taking the tics
one at a time gives a smoother animation; the summed rewards and episode
boundaries are checked against the recorded CSVs. make_gifs.py assembles the
web animation from OUT.
"""
import csv, sys
from pathlib import Path
import vizdoom as vzd

out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)
for label in ('before', 'after'):
    with open(label + '-actions.csv') as f:
        actions = list(csv.DictReader(f))
    game = vzd.DoomGame()
    game.load_config(str(Path(vzd.scenarios_path) / 'basic.cfg'))
    game.set_window_visible(False)
    game.set_screen_format(vzd.ScreenFormat.RGB24)
    game.set_screen_resolution(vzd.ScreenResolution.RES_160X120)
    game.set_seed(1000)
    game.init()
    game.new_episode()
    frames = [game.get_state().screen_buffer.copy()]
    for row in actions:
        buttons = [0] * len(game.get_available_buttons())
        buttons[int(row['action'])] = 1
        reward = 0.
        for _ in range(4):
            if game.is_episode_finished():
                break
            reward += game.make_action(buttons, 1)
            state = game.get_state()
            if state is not None:
                frames.append(state.screen_buffer.copy())
        assert reward == float(row['reward']), (label, row, reward)
        assert game.is_episode_finished() == (row['terminated'] == 'true' or row['truncated'] == 'true')
    assert game.is_episode_finished()
    game.close()
    for i, pixels in enumerate(frames):
        (out / f'{label}-{i:04d}.ppm').write_bytes(b'P6\n160 120\n255\n' + pixels.tobytes())
    print(f'{label}: {len(actions)} actions, {len(frames)} frames; rewards and boundaries match')
