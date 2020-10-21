import math
import numpy as np
from PIL import Image
import glob
import sys
import os.path

def to_image_array(data):
    data = np.copy(data)
    # 获取矩阵（图像）的长宽
    rows, cols = data.shape
    for i in range(rows):
        for j in range(cols):
            # 与阈值比较
            if data[i, j] == 0:
                # 设为灰度最小值
                data[i, j] = 0
            else:
                # 设为灰度最大值
                data[i, j] = 255

    return data

def show_image(data):
    data = to_image_array(data)
    img = Image.fromarray(data)
    img.save('temp/tmp.jpg')
    img.show()

def save_image(data, index=0):
    data = to_image_array(data)
    img = Image.fromarray(data)
    img.save('temp/nums/{}.jpg'.format(index))


def strip_row(data):
    # 每行最小值
    row_min = np.min(data, axis=1)
    # 找到第一个有图像的行
    row_start = np.argmin(row_min)
    # 找到最后一个有图像的行
    row_end = np.argmin(np.flip(row_min))
    # 只取有图像的行
    if row_end > 0:
        data = data[row_start:-row_end, :]
    else:
        data = data[row_start:, :]

    return data

def strip_col(data):
    # 每行最小值
    col_min = np.min(data, axis=0)
    # 找到第一个有图像的行
    col_start = np.argmin(col_min)
    # 找到最后一个有图像的行
    col_end = np.argmin(np.flip(col_min))
    # 只取有图像的行
    if col_end > 0:
        data = data[:, col_start:-col_end]
    else:
        data = data[:, col_start:]

    return data

def is_valid_num(data):
    # 每行最小值
    col_min = np.min(data, axis=0)
    # 找到第一个有图像的行
    col_start = np.argmin(col_min)
    return col_start < 50

def normal_image(data, revert = False):
    # 获取矩阵（图像）的长宽
    data = np.copy(data)
    rows, cols = data.shape
    for i in range(rows):
        for j in range(cols):
            # 与阈值比较
            if data[i, j] <= 220:
                # 设为灰度最小值
                if revert:
                    data[i, j] = 0
                else:
                    data[i, j] = 1
            else:
                # 设为灰度最大值
                if revert:
                    data[i, j] = 1
                else:
                    data[i, j] = 0

    return data

def find_num_poses(data):
    poses = [0]
    for pos in range(4, data.shape[1]):
        img_matrix_spited = data[:, :pos]
        col_min = np.min(img_matrix_spited, axis=0)
        col_end = np.argmin(np.flip(col_min))
        if col_end > 0:
            rel_pos = pos - col_end
            if rel_pos not in poses:
                poses.append(rel_pos)

    return poses

def load_datasets():
    datasets = []
    for img_path in glob.glob('datasets/*.jpg'):
        data = np.array(Image.open(img_path).convert("L"))
        data = normal_image(data, True)
        num = img_path[9:-4]
        if num.startswith('point'):
            num = '.'
        if num.startswith('split'):
            num = ' '

        datasets.append((num[0], data.flatten()))

    return datasets

def load_worlds():
    worlds = ['minecraft:overworld', 'minecraft:the_nether', 'minecraft:the_end']
    datasets = []
    for world in worlds:
        img_path = 'datasets/{}.jpg'.format(world.split(':', 1)[1])
        data = np.array(Image.open(img_path).convert("L"))
        data = normal_image(data, True)
        datasets.append((world, data.flatten()))

    return datasets


def recog_one(data, datasets):
    x = data.flatten()
    got = -1
    score = -1
    for num, dataset in datasets:
        try:
            new_score = np.sum(x ^ dataset)
            if score == -1:
                score = new_score
                got = num

            if score > new_score:
                score = new_score
                got = num
        except Exception:
            pass

    return got, score


def read_image(img_path):
    data = np.array(Image.open(img_path).convert("L"))

    data = normal_image(data)
    data = strip_row(data)
    data = strip_col(data)
    return data


def recog_num(img_path):
    datasets = load_datasets()

    img_matrix = read_image(img_path)

    poses = find_num_poses(img_matrix)

    poses_start = poses[0:-1]
    poses_end = poses[1:]

    got_nums = []
    for col_start, col_end in zip(poses_start, poses_end):
        img_matrix_spited = img_matrix[:, col_start:col_end]
        if not is_valid_num(img_matrix_spited):
            continue
        img_matrix_spited = strip_col(img_matrix_spited)
        x = img_matrix_spited.flatten()
        got_num, score = recog_one(img_matrix_spited, datasets)

        if score == 0:
            got_nums.append(got_num)
        else:
            print(got_num, score)
            save_image(img_matrix_spited, '{}{}-{}'.format(os.path.basename(img_path), col_start, col_end))


    print(''.join(got_nums))

def recog_world(img_path):
    worlds = load_worlds()
    img_matrix = read_image(img_path)
    got_world, score = recog_one(img_matrix, worlds)

    if score == 0:
        print(got_world)
    else:
        print('error')


def main(script, img_path, recog_type='pos'):
    if recog_type == 'pos':
        recog_num(img_path)
    else:
        recog_world(img_path)

main(*sys.argv)
