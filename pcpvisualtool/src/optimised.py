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
from multiprocessing import Queue

input_path="../input/"
# output_path="../output/"
# dataname="3DML_urban_point_cloud.xyz"
point_cloud = o3d.data.PLYPointCloud()
pcd = o3d.io.read_point_cloud(point_cloud.path)

skel_data_name = "dataset.npy"
skel_data = np.load(input_path+skel_data_name)
#skeleton_data = rand_trans_poses(skel_data)
skeleton_data = skel_data


def skeleton_poses(skeletons):
  N, P, L = skeletons.shape
  # skeletons shape = (N, 15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
  skeletons = np.reshape(skeletons, newshape=(N*P, L))
  # skeletons shape = (N*15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
  lines = np.asarray([[0,1], [1,2], [2,3], [3,4], [1,5], [5,6], [6,7], [8,9], [9,10], [11,12], [12,13], [1,14], [8,14], [11,14]], dtype=int)
  # lines shape = (14, 2)
  lines = np.expand_dims(lines, axis=0)
  # lines shape = (1, 14, 2)
  lines = np.repeat(lines, repeats=N, axis=0)
  # lines shape = (N, 14, 2)
  summation = np.expand_dims(np.expand_dims(np.arange(0, 15*(N-1)+1, 15, dtype=int), axis=1), axis=1)
  # summation shape = (N, 1, 1)
  lines = lines + summation
  # lines shape = (N, 14, 2)
  lines = np.reshape(lines, (lines.shape[0]*lines.shape[1], lines.shape[2]))
  # lines shape = (N*14, 2)
  lines = o3d.utility.Vector2iVector(lines)
  line_set = o3d.geometry.LineSet()
  line_set.lines = lines
  line_set.points = o3d.utility.Vector3dVector(skeletons)
  return line_set



def gui(queue, *args):
    
    # CREATION OF THE NON-BLOCKING OPEN3D SCENE
    vis = o3d.visualization.Visualizer()
    vis.create_window()
    
    
    line_set = o3d.geometry.LineSet()
    
    
    mesh_box = o3d.geometry.TriangleMesh.create_box(width=20.0,
                                                height=0.1,
                                                depth=20.0)
    mesh_box.paint_uniform_color([0.8, 0.8, 0.8])
    mesh_box.translate(-mesh_box.get_center())
    mesh_box.translate([0,-0.05,0])
    
    
    vis.add_geometry(line_set)
    vis.add_geometry(mesh_box)

    
    keyboard.send('alt', True, False)
    
    ctr = vis.get_view_control()
    parameters = o3d.io.read_pinhole_camera_parameters("ScreenCamera_2022-11-25-12-27-51.json")
    ctr.convert_from_pinhole_camera_parameters(parameters)
    
    
    grid = generate_axis(10, 10)
    vis.add_geometry(grid)
    
    counter = 0
    
    while (True):
        counter = counter + 1
        
        if counter % 1000 == 0 or counter == 10: queue.put(skeletons_detected(3, skeleton_data))  #TESTING 
        
        start_time = time.time()
        if not queue.empty():
            #vis.clear_geometries()
            poses = queue.get()
            # poses shape = (N, 15, 3) where N=people detected, 15=human body keypoints, 3=(x,y,z)
            updated_line_set = skeleton_poses(poses)
            line_set.lines = updated_line_set.lines
            line_set.points = updated_line_set.points
            
            # Create an array of the desired RGB values
            rgb_values = np.array([[0,0,0],
                                   [0,0,255], # right shoulder
                                   [0,0,255], # right arm
                                   [0,0,255], # right hand
                                   [255,0,0], # left shoulder
                                   [255,0,0], # left arm
                                   [255,0,0], # left hand
                                   [0,0,255], # right leg
                                   [0,0,255], # right foot
                                   [255,0,0], # left leg
                                   [255,0,0], # left foot
                                   [0,0,0], # torso
                                   [0,0,255], # right thigh
                                   [255,0,0]]) # left thigh
            
            # Use np.tile() to repeat the RGB values in the desired shape
            repeated_values = np.tile(rgb_values, (3, 1))
            print(np.shape(line_set.lines))
            line_set.colors = o3d.utility.Vector3dVector(repeated_values)
            
            vis.update_geometry(line_set)
            
        if keyboard.is_pressed('q'):
            vis.destroy_window()
            keyboard.send('alt', False, True)
        
        vis.poll_events()
        vis.update_renderer()
        
        print("FPS: ", 1.0 / (time.time() - start_time))  #TESTING
        

def generate_axis(size, num_cells):
    # Set up the grid
    nx, ny = num_cells, num_cells
    x = np.linspace(-size, size, nx)
    y = np.linspace(-size, size, ny)
    xv, yv = np.meshgrid(x, y)
    
    # Create the grid points
    points = np.stack((xv.flatten(), yv.flatten(), np.zeros(nx * ny)), axis=-1)
    
    # Rotate the points so the grid is horizontal
    rotation_matrix = np.array([[1, 0, 0],
                                 [0, 0, 1],
                                 [0, -1, 0]])
    points = np.dot(points, rotation_matrix.T)
    
    # Create a point cloud and draw the grid
    pcd = o3d.geometry.PointCloud()
    pcd.points = o3d.utility.Vector3dVector(points)
    
    origin = np.array([[0, 0, 0]])
    point = np.array([[0, 10, 0]])
    points = np.concatenate((points, origin, point), axis=0)
    
    
    # Create a list of line segments to connect the points
    lines = []
    
    lines.append([points.size-1,points.size-2])
    
    for i in range(nx):
        for j in range(ny):
            if i < nx - 1:
                lines.append([i * ny + j, (i + 1) * ny + j])
            if j < ny - 1:
                lines.append([i * ny + j, i * ny + j + 1])
    lines = np.array(lines)
    
    # Create a line set and draw the grid lines
    grid = o3d.geometry.LineSet()
    grid.points = pcd.points
    grid.lines = o3d.utility.Vector2iVector(lines)
    
    return grid
            
            
            

# Use this function to create a given number of skeleton poses in a room of 400 mq where the robot is placed
# at the origin
def skeletons_detected(n_skeletons, dataset):
  # n_skeletons is an integer number. It is the number of skeleton poses returned by this function
  # dataset is a dataset of frame poses whose shape is (N,15,3) where N=frames, 15=human body keypoints, 3=(x,y,z)
  translations = np.random.randint(low=-10, high=11, size=(dataset.shape[0], 1, dataset.shape[2]))
  # translation shape = (N,1,3)
  translations[:,:,1] = 0 # set the height translation to 0
  poses = dataset + translations
  np.random.shuffle(poses)
  return poses[:n_skeletons]

queue = Queue()

    
gui(queue)
    
    


            
