from pathlib import Path
import os


def _find_data_directory():
    current_directory = Path(os.path.dirname(__file__))

    while current_directory != current_directory.root:
        children = [d for d in current_directory.iterdir() if d.is_dir()]
        if any(x.name == '.git' for x in children):
            break
        current_directory = current_directory.parent

    return current_directory.absolute()


DATA_FILE_PATH = '%s/data' % (_find_data_directory())


def read_advent_data_file(path, day=1):
    global DATA_FILE_PATH
    if not (path.startswith('Day') or path.startswith('day')):
        path = 'Day%d/%s' % (day, path)

    with open('%s/%s' % (DATA_FILE_PATH, path), 'r') as data_file:
        return data_file.read()
