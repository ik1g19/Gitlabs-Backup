# -*- coding: utf-8 -*-
"""
Created on Thu Dec 15 18:04:46 2022

@author: isaac
"""

import numpy as np
import open3d as o3d
import open3d.visualization.gui as gui

class Viewer3D(object):
    
    frame_counter = 0
    pcd = None

    def __init__(self, title):
        app = gui.Application.instance
        app.initialize()

        self.main_vis = o3d.visualization.O3DVisualizer(title)
        app.add_window(self.main_vis)
        
        self.setup_depth_streaming()
        self.setup_geometry()
        self.setup_o3d_scene()

    def setup_depth_streaming(self):
        pass

    def setup_geometry(self):
        # setup an empty point cloud to be later populated with live data
        self.skeletons = o3d.geometry.LineSet()
        # the name is necessary to remove from the scene
        self.skeletons_name = "skeletons"
        self.slam_name = "slam"
        
        self.mesh_box = o3d.geometry.TriangleMesh.create_box(width=20.0,
                                                    height=0.1,
                                                    depth=20.0)
        self.mesh_box.paint_uniform_color([0.8, 0.8, 0.8])
        self.mesh_box.translate(-self.mesh_box.get_center())
        self.mesh_box.translate([0,-0.05,0])
        
        self.axis = SceneTools.generate_axis(10, 10)
        
        self.skeleton_data = np.load("../input/dataset.npy")
        
    def update_point_clouds(self, slam = None):
        self.frame_counter = self.frame_counter + 1
        
        if self.frame_counter == 10 or self.frame_counter % 500 == 0:
            # update your point cloud data here: convert depth to point cloud / filter / etc.
            poses = Skeleton3D.skeletons_detected(3, self.skeleton_data)
            
            self.skeletons = Skeleton3D.skeleton_poses(poses)
            
            self.skeletons.colors = Skeleton3D.skeleton_paint(3)    
            
            self.pcd = o3d.io.read_point_cloud(slam) if slam is not None else None

    def setup_o3d_scene(self):
        self.main_vis.add_geometry(self.skeletons_name, self.skeletons)
        self.main_vis.add_geometry("base", self.mesh_box)
        self.main_vis.add_geometry("axis", self.axis)
        
        self.main_vis.reset_camera_to_default()
        # center, eye, up
        self.main_vis.setup_camera(60,
                                    [0, 0, 0],
                                    [0, 10, 0],
                                    [0, 1, 0])
        
        self.main_vis.enable_raw_mode(True)

    def update_o3d_scene(self):
        self.main_vis.remove_geometry(self.skeletons_name)
        if self.pcd is not None: self.main_vis.remove_geometry(self.slam_name)
        self.main_vis.add_geometry(self.skeletons_name, self.skeletons)
        if self.pcd is not None: self.main_vis.add_geometry(self.slam_name, self.pcd)

    def run_one_tick(self):
        app = o3d.visualization.gui.Application.instance
        tick_return = app.run_one_tick()
        if tick_return:
            self.main_vis.post_redraw()
        return tick_return
    
    
    
    
    
    
class Skeleton3D:
    @staticmethod
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
  
    # Use this function to create a given number of skeleton poses in a room of 400 mq where the robot is placed
    # at the origin
    @staticmethod
    def skeletons_detected(n_skeletons, dataset):
        # n_skeletons is an integer number. It is the number of skeleton poses returned by this function
        # dataset is a dataset of frame poses whose shape is (N,15,3) where N=frames, 15=human body keypoints, 3=(x,y,z)
        translations = np.random.randint(low=-10, high=11, size=(dataset.shape[0], 1, dataset.shape[2]))
        # translation shape = (N,1,3)
        translations[:,:,1] = 0 # set the height translation to 0
        poses = dataset + translations
        np.random.shuffle(poses)
        return poses[:n_skeletons]
  
    @staticmethod
    def skeleton_paint(num_of_skeletons):
        return o3d.utility.Vector3dVector(np.tile(Skeleton3D.rgb_values, (num_of_skeletons, 1)))
  
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
  
    
    
    
    
  
class SceneTools:
    @staticmethod
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
    
    
    
    
    
###############################################################################
###############################################################################    
    
    
    
    
    

viewer3d = Viewer3D("point cloud gui")

try:
    while True:
        # Step 1) Perturb the cloud with a random walk to simulate an actual read
        # (based on https://github.com/isl-org/Open3D/blob/master/examples/python/visualization/multiple_windows.py)
        viewer3d.update_point_clouds()
        # Step 2) Update the cloud and tick the GUI application
        viewer3d.update_o3d_scene()
        viewer3d.run_one_tick()
except Exception as e:
    print(e)