import random
from math import atan2, pi

from matplotlib import pyplot as plt
from matplotlib.patches import Ellipse
from scipy.spatial import distance


def drawEllipse_full(e_posi, extra_posi, ka, kb, ellipseColor_r = 'white', ellipseColor_t = 'white',
                     extra_disc_color = 'orangered', ellipsetransp = 0.5, savefig = False, plot_axis_limit_fixed = False, zoomin = False):
    """
    This function allows to draw more than one ellipse. The parameter is
    a list of coordinate (must contain at least two coordinates)
    The radial and tangential ellipses for the same coordinates are drawn.
    plot_axis_limit_fixed: False 根据最外面的点位置设定x，y长度
    zoomin： Ture：只看局部（有点）的画面
    """
    eccentricities = []
    for i in range(len(e_posi)):
        eccentricities0 = distance.euclidean(e_posi[i], (0, 0))
        eccentricities.append(eccentricities0)
    # radial
    angle_deg = []
    for ang in range(len(e_posi)):
        angle_rad0 = atan2(e_posi[ang][1], e_posi[ang][0])
        angle_deg0 = angle_rad0 * 180 / pi
        angle_deg.append(angle_deg0)
    my_e = [Ellipse(xy = e_posi[j], width = eccentricities[j] * ka * 2, height = eccentricities[j] * kb * 2,
                    angle = angle_deg[j])
            for j in range(len(e_posi))]

    # tangential
    angle_deg2 = []
    for ang in range(len(e_posi)):
        angle_rad0_2 = atan2(e_posi[ang][1], e_posi[ang][0])
        angle_deg0_2 = angle_rad0_2 * 180 / pi + 90
        angle_deg2.append(angle_deg0_2)
    my_e2 = [Ellipse(xy = e_posi[j], width = eccentricities[j] * ka * 2, height = eccentricities[j] * kb * 2,
                     angle = angle_deg[j] + 90)
             for j in range(len(e_posi))]

    fig, ax = plt.subplots(subplot_kw = {'aspect': 'equal'}, figsize = (4, 3))
    for e in my_e:
        ax.add_artist(e)
        e.set_clip_box(ax.bbox)
        e.set_alpha(ellipsetransp)
        e.set_facecolor(ellipseColor_r)
    for e2 in my_e2:
        ax.add_artist(e2)
        e2.set_clip_box(ax.bbox)
        e2.set_alpha(ellipsetransp)
        e2.set_facecolor(ellipseColor_t)

    # show the discs on the ellipses-flower
    for dot in e_posi:
        plt.plot(dot[0], dot[1], color = 'k', marker = 'o', markersize = 2)
    # plt.show()
    for dot1 in extra_posi:
        plt.plot(dot1[0], dot1[1], color = extra_disc_color, marker = 'o', markersize = 2)
    plt.plot(0, 0, color = 'red', marker = '+', markersize = 4)
    # plt.show()
    # ax.set_xlim([-800, 800])
    # ax.set_ylim([-500, 500])

    # ax.set_title('wS_%s_eS_%s_%s_E.png' %(newWindowSize,ka,kb))

    if plot_axis_limit_fixed:
        ax.set_xlim([-533, 533])
        ax.set_ylim([-300, 300])
    else:

        all_x = [p[0] for p in e_posi + extra_posi] + [0]
        all_y = [p[1] for p in e_posi + extra_posi] + [0]

        max_x = max(abs(x) for x in all_x)
        max_y = max(abs(y) for y in all_y)

        # 保证所有点都在显示区域内
        margin_ratio = 1.05
        max_x *= margin_ratio
        max_y *= margin_ratio

        # 保持 16:9 比例并扩大范围
        aspect_ratio = 16 / 9
        if max_x / max_y >= aspect_ratio:
            half_width = max_x
            half_height = half_width / aspect_ratio
        else:
            half_height = max_y
            half_width = half_height * aspect_ratio

        ax.set_xlim(-half_width, half_width)
        ax.set_ylim(-half_height, half_height)

    if zoomin:
        all_x = [p[0] for p in e_posi + extra_posi]
        all_y = [p[1] for p in e_posi + extra_posi]

        padding_x = (max(all_x) - min(all_x)) * 0.1 + 10
        padding_y = (max(all_y) - min(all_y)) * 0.1 + 10

        ax.set_xlim(min(all_x) - padding_x, max(all_x) + padding_x)
        ax.set_ylim(min(all_y) - padding_y, max(all_y) + padding_y)

    # 边框不可见
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.spines['left'].set_visible(False)
    # 坐标不可见
    ax.axes.get_yaxis().set_visible(False)
    ax.axes.get_xaxis().set_visible(False)
    ax.patch.set_facecolor('lightgray')
    plt.show()
    if savefig:
        fig.savefig('efull%s_zoomin%s.svg' % (str(e_posi)[0:15], zoomin), bbox_inches = 'tight', pad_inches = 0)
