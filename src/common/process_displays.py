import math

import numpy as np


def limit_posi_to_sector(sector_angle_in_deg, direction_in_deg, positions):
    # converts sector angle and direction into radians
    angle_rad = math.radians(sector_angle_in_deg) / 2  # Half on each side
    direction_rad = math.radians(direction_in_deg) % (
            2 * math.pi)  # ensure direction_rad is in [0, 2pi]

    sector_positions = []
    for x, y in positions:
        # converts Cartesian to polar angle theta(in radians)
        theta = math.atan2(y, x)
        theta = (theta + 2 * math.pi) % (2 * math.pi)

        # calcualte minimal angular difference between point angle and sector direction
        diff = abs(((theta - direction_rad + math.pi) % (2 * math.pi)) - math.pi)
        # if within the sector wedge, keep the point
        if diff <= angle_rad:
            sector_positions.append((x, y))

    return sector_positions


def get_len (posi_list):
    return len(posi_list)


def mirror_coords(coords):
    """
    将给定的坐标列表关于 x=0 轴（纵轴）做镜像变换。

    input: coords (list of tuple): 原始坐标列表，每个元素为 (x, y)。

    list of tuple: (-x, y)。
    """
    return [(-x, y) for x, y in coords]


def remove_near_origin(pos_list, radius=100):
    return [pos for pos in pos_list if np.hypot(pos[0], pos[1]) >= radius]


def remove_outside_radius(pos_list, radius=530):
    return [pos for pos in pos_list if np.hypot(pos[0], pos[1]) <= radius]
