"""
Created on Wed Nov  2 12:04:06 2022

@author: isaac
"""

import numpy as np
import open3d as o3d
import open3d.visualization.gui as GUI
import keyboard
from multiprocessing.connection import Listener



def skeleton_poses(skeletons):
    N, P, L = skeletons.shape
    # skeletons shape = (N, 15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
    skeletons = np.reshape(skeletons, newshape=(N * P, L))
    # skeletons shape = (N*15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
    lines = np.asarray(
        [[0, 1], [1, 2], [2, 3], [3, 4], [1, 5], [5, 6], [6, 7], [8, 9], [9, 10], [11, 12], [12, 13], [1, 14], [8, 14],
         [11, 14]], dtype=int)
    # lines shape = (14, 2)
    lines = np.expand_dims(lines, axis=0)
    # lines shape = (1, 14, 2)
    lines = np.repeat(lines, repeats=N, axis=0)
    # lines shape = (N, 14, 2)
    summation = np.expand_dims(np.expand_dims(np.arange(0, 15 * (N - 1) + 1, 15, dtype=int), axis=1), axis=1)
    # summation shape = (N, 1, 1)
    lines = lines + summation
    # lines shape = (N, 14, 2)
    lines = np.reshape(lines, (lines.shape[0] * lines.shape[1], lines.shape[2]))
    # lines shape = (N*14, 2)
    lines = o3d.utility.Vector2iVector(lines)
    line_set = o3d.geometry.LineSet()
    line_set.lines = lines
    line_set.points = o3d.utility.Vector3dVector(skeletons)
    return line_set


def gui():
    conn = None
    listener = None
    vis = None
    try:
        address = ('localhost', 6000)  # family is deduced to be 'AF_INET'
        listener = Listener(address, authkey=None)
        conn = listener.accept()
        # CREATION OF THE NON-BLOCKING OPEN3D SCENE
        vis = o3d.visualization.Visualizer()
        vis.create_window()

        line_set = o3d.geometry.LineSet()

        mesh_box = o3d.geometry.TriangleMesh.create_box(width=40.0,
                                                        height=0.1,
                                                        depth=40.0)
        mesh_box.paint_uniform_color([0.8, 0.8, 0.8])
        mesh_box.translate(-mesh_box.get_center())
        mesh_box.translate([0, -0.2, 0])

        vis.add_geometry(line_set)
        vis.add_geometry(mesh_box)

        keyboard.send('alt', True, False)

        ctr = vis.get_view_control()
        parameters = o3d.io.read_pinhole_camera_parameters("ScreenCamera_2022-11-25-12-27-51.json")
        ctr.convert_from_pinhole_camera_parameters(parameters)

        axis = o3d.geometry.LineSet()
        axis_points = o3d.utility.Vector3dVector(
            np.asarray([[0, 0, 0], [0, 20, 0], [-20, 0, 0], [20, 0, 0], [0, 0, -20], [0, 0, 20]]))
        axis_lines = o3d.utility.Vector2iVector(np.asarray([[0, 1], [2, 3], [4, 5]]))
        axis.points = axis_points
        axis.lines = axis_lines

        vis.add_geometry(axis)

        while (True):
            poses = conn.recv()
            # poses shape = (N, 15, 3) where N=people detected, 15=human body keypoints, 3=(x,y,z)
            if poses == 'close':
                conn.close()
                listener.close()
                vis.destroy_window()
                keyboard.send('alt', False, True)
                break
            updated_line_set = skeleton_poses(poses)
            line_set.lines = updated_line_set.lines
            line_set.points = updated_line_set.points
            vis.update_geometry(line_set)
            if keyboard.is_pressed('q'):
                conn.close()
                listener.close()
                vis.destroy_window()
                keyboard.send('alt', False, True)
                break
            vis.poll_events()
            vis.update_renderer()
    except:
        print("Fail")
        conn.close()
        listener.close()
        vis.destroy_window()
        keyboard.send('alt', False, True)

##################GUI2############################

# Container for methods to initialise the scene
class Viewer3D(object):

    def __init__(self, title):
        app = GUI.Application.instance
        app.initialize()

        self.main_vis = o3d.visualization.O3DVisualizer(title)
        app.add_window(self.main_vis)

        self.setup_depth_streaming()
        self.setup_geometry()
        self.setup_o3d_scene()

        self.main_vis.enable_raw_mode(True)

    def setup_depth_streaming(self):
        pass

    # creates intial geometry for the scene
    def setup_geometry(self):
        # setup an empty lineset to be later populated with live data
        self.skeletons = o3d.geometry.LineSet()
        # the name is necessary to remove from the scene
        self.skeletons_name = "skeletons"

        # create a plane
        self.mesh_box = o3d.geometry.TriangleMesh.create_box(width=20.0,
                                                             height=0.1,
                                                             depth=20.0)
        self.mesh_box.paint_uniform_color([0.8, 0.8, 0.8])
        self.mesh_box.translate(-self.mesh_box.get_center())
        self.mesh_box.translate([0, -0.05, 0])
        
        # create a grid
        self.axis = SceneTools.generate_axis(10, 10)

    # populates the lineset with human poses every frame
    # poses - np array of pose data
    def update_poses(self, poses):
        # poses shape = (N,15,3) where N=people detected, 15=body keypoints, 3=(x,y,z)
        N, _, _ = poses.shape
        self.skeletons = Skeleton3D.skeleton_poses(poses)
        self.skeletons.colors = Skeleton3D.skeleton_paint(N)

    # add geometry to the scene and initialise camera
    def setup_o3d_scene(self):
        self.main_vis.add_geometry(self.skeletons_name, self.skeletons)
        self.main_vis.add_geometry("base", self.mesh_box)
        self.main_vis.add_geometry("axis", self.axis)
        
        self.main_vis.reset_camera_to_default()
        # last 3 params - center, eye, up
        self.main_vis.setup_camera(60,
                                   [4, 2, 5],
                                   [0, 0, -1.5],
                                   [0, 1, 0])

    # changes the pose geometry in the scene every frame
    def update_o3d_scene(self):
        self.main_vis.remove_geometry(self.skeletons_name)
        self.main_vis.add_geometry(self.skeletons_name, self.skeletons)

    def run_one_tick(self):
        app = o3d.visualization.gui.Application.instance
        tick_return = app.run_one_tick()
        if tick_return:
            self.main_vis.post_redraw()
        return tick_return

    def quit(self):
        self.app.quit()


# container for methods for creating human poses
class Skeleton3D:
    # skeletons - np array containing pose data
    # return - lineset of poses
    @staticmethod
    def skeleton_poses(skeletons):
        N, P, L = skeletons.shape
        # skeletons shape = (N, 15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
        skeletons = np.reshape(skeletons, newshape=(N * P, L))
        # skeletons shape = (N*15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
        lines = np.asarray(
            [[0, 1], [1, 2], [2, 3], [3, 4], [1, 5], [5, 6], [6, 7], [8, 9], [9, 10], [11, 12], [12, 13], [1, 14],
             [8, 14], [11, 14]], dtype=int)
        # lines shape = (14, 2)
        lines = np.expand_dims(lines, axis=0)
        # lines shape = (1, 14, 2)
        lines = np.repeat(lines, repeats=N, axis=0)
        # lines shape = (N, 14, 2)
        summation = np.expand_dims(np.expand_dims(np.arange(0, 15 * (N - 1) + 1, 15, dtype=int), axis=1), axis=1)
        # summation shape = (N, 1, 1)
        lines = lines + summation
        # lines shape = (N, 14, 2)
        lines = np.reshape(lines, (lines.shape[0] * lines.shape[1], lines.shape[2]))
        # lines shape = (N*14, 2)
        lines = o3d.utility.Vector2iVector(lines)
        line_set = o3d.geometry.LineSet()
        line_set.lines = lines
        line_set.points = o3d.utility.Vector3dVector(skeletons)
        return line_set

    # colours joints of poses according to rgb_values
    @staticmethod
    def skeleton_paint(num_of_skeletons):
        return o3d.utility.Vector3dVector(np.tile(Skeleton3D.rgb_values, (num_of_skeletons, 1)))

    rgb_values = np.array([[0, 0, 0],
                           [0, 0, 255],  # right shoulder
                           [0, 0, 255],  # right arm
                           [0, 0, 255],  # right hand
                           [255, 0, 0],  # left shoulder
                           [255, 0, 0],  # left arm
                           [255, 0, 0],  # left hand
                           [0, 0, 255],  # right leg
                           [0, 0, 255],  # right foot
                           [255, 0, 0],  # left leg
                           [255, 0, 0],  # left foot
                           [0, 0, 0],  # torso
                           [0, 0, 255],  # right thigh
                           [255, 0, 0]])  # left thigh



# container for helper methods for populating the scene
class SceneTools:
    # create a grid
    # size - will create a grid of size x size meters
    # num_cells - grid will be divided into num_cells on each axis
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

        lines.append([points.size - 1, points.size - 2])

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


def gui2():
    conn = None
    listener = None
    try:
        address = ('localhost', 6000)  # family is deduced to be 'AF_INET'
        listener = Listener(address, authkey=None)
        conn = listener.accept()
        viewer3d = Viewer3D("Visualisation")

        while (True):
            poses = conn.recv()
            # poses shape = (N, 15, 3) where N=people detected, 15=human body keypoints, 3=(x,y,z)
            if poses == 'close':
                conn.close()
                listener.close()
                viewer3d.quit()
                keyboard.send('alt', False, True)
                break
            # Step 1) Perturb the cloud with a random walk to simulate an actual read
            # (based on https://github.com/isl-org/Open3D/blob/master/examples/python/visualization/multiple_windows.py)
            viewer3d.update_poses(poses)
            # Step 2) Update the cloud and tick the GUI application
            viewer3d.update_o3d_scene()
            viewer3d.run_one_tick()
            if keyboard.is_pressed('q'):
                conn.close()
                listener.close()
                viewer3d.quit()
                keyboard.send('alt', False, True)
                break
    except:
        conn.close()
        listener.close()
        viewer3d.quit()
        keyboard.send('alt', False, True)

gui2()


