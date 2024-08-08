# -*- coding: utf-8 -*-
"""
Created on Wed Nov  2 12:04:06 2022

@author: isaac
"""

import numpy as np
import open3d as o3d
import open3d.visualization.gui as gui
import open3d.visualization.rendering as rendering
import platform
import random
import threading
import time
import keyboard
import copy
            
            
            
            
input_path="../input/"
# output_path="../output/"
dataname="3DML_urban_point_cloud.xyz"
point_cloud = o3d.data.PLYPointCloud()
pcd = o3d.io.read_point_cloud(point_cloud.path)

#skel_data_name = "dataset.npy"
#skel_data = np.load(input_path+skel_data_name)
#skeleton_data = rand_trans_poses(skel_data)
#skeleton_data = skel_data

o3d.visualization.draw_geometries([pcd])