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
        imag_matrix_spited = data[:, :pos]
        col_min = np.min(imag_matrix_spited, axis=0)
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
            num = '/'

        datasets.append((num[0], data.flatten()))

    return datasets

def my_sum(x, y):
    if len(x) != len(y):
        return 1000
    score = 0
    for x0, y0 in zip(x, y):
        if x0 != y0:
            score += 1

    return score

def main(script, img_path):
    datasets = load_datasets()
    img_matrix = np.array(Image.open(img_path).convert("L"))

    img_matrix = normal_image(img_matrix)
    img_matrix = strip_row(img_matrix)
    img_matrix = strip_col(img_matrix)

    poses = find_num_poses(img_matrix)

    poses_start = poses[0:-1]
    poses_end = poses[1:]

    got_nums = []
    for col_start, col_end in zip(poses_start, poses_end):
        imag_matrix_spited = img_matrix[:, col_start:col_end]
        if not is_valid_num(imag_matrix_spited):
            continue
        imag_matrix_spited = strip_col(imag_matrix_spited)
        x = imag_matrix_spited.flatten()
        # save_image(imag_matrix_spited, '{}-{}'.format(col_start, col_end))
        got_num = -1
        score = -1
        for num, dataset in datasets:
            try:
                new_score = my_sum(x, dataset)
                if score == -1:
                    score = new_score
                    got_num = num

                if score > new_score:
                    score = new_score
                    got_num = num
            except Exception:
                pass


        if score == 0:
            got_nums.append(got_num)
        else:
            print(got_num, score)
            save_image(imag_matrix_spited, '{}{}-{}'.format(os.path.basename(img_path), col_start, col_end))


    print(''.join(got_nums))

main(*sys.argv)
