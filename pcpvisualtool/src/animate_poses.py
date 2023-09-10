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
# dataname="3DML_urban_point_cloud.xyz"
point_cloud = o3d.data.PLYPointCloud()
pcd = o3d.io.read_point_cloud(point_cloud.path)

skel_data_name = "dataset.npy"
skel_data = np.load(input_path+skel_data_name)
#skeleton_data = rand_trans_poses(skel_data)
skeleton_data = skel_data



# converts a set of poses to a set of linesets
# param - dataset of poses
# return - pose as lineset
def pose_lineset(pose):
  # skeletons shape = (N, 15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
  lines = [[0,1], [1,2], [2,3], [3,4], [1,5], [5,6], [6,7], [8,9], [9,10], [11,12], [12,13], [1,14], [8,14], [11,14]]
  lines = o3d.utility.Vector2iVector(lines)

  points = o3d.utility.Vector3dVector(pose)
    #line_set.scale(100.0,line_set.get_center())
  return points, lines



# fetch random pose from dataset along with index
# param - dataset of poses
# return - tuple of pose and corresponding index in pose array
def rand_pose_index(dataset):
  index = np.random.randint(0,dataset.shape[0])
  pose = dataset[index]
  return [pose, index]



# randomly translate poses in the pose array
# param - dataset of poses
# return - translated dataset
def rand_trans_poses(dataset):
    # n_skeletons is an integer number. It is the number of skeleton poses returned by this function
    # dataset is a dataset of frame poses whose shape is (N,15,3) where N=frames, 15=human body keypoints, 3=(x,y,z)
    translations = np.random.randint(low=-10, high=11, size=(dataset.shape[0], 1, dataset.shape[2]))
    # translation shape = (N,1,3)
    translations[:,:,1] = 0 # set the height translation to 0
    return dataset + translations




vis = o3d.visualization.Visualizer()
vis.create_window()


#to_render = skeleton_poses(skeletons_detected(3,skeleton_data))



people = []

for n in range(0,5):
    pose_with_index = rand_pose_index(skeleton_data)
    line_set = o3d.geometry.LineSet()
    people.append([pose_with_index,line_set])
    vis.add_geometry(line_set)
    
def skeleton_poses(skeletons):
  # skeletons shape = (N, 15, 3) where N=people, 15=human body keypoints, 3=(x,y,z)
  line_sets = []
  lines = [[0,1], [1,2], [3,4], [1,5], [5,6], [6,7], [8,9], [9,10], [11,12], [12,13], [1,14], [8,14], [11,14]]
  lines = o3d.utility.Vector2iVector(lines)
  for skeleton in skeletons:
    line_set = o3d.geometry.LineSet()
    line_set.points = o3d.utility.Vector3dVector(skeleton)
    line_set.lines = lines
    line_sets.append(line_set)
  return line_sets


def gui(queue, *args):
    # CREATION OF THE NON-BLOCKING OPEN3D SCENE
    vis = o3d.visualization.Visualizer()
    vis.create_window()
    while (True):
        if not queue.empty():
            vis.clear_geometries()
            poses = queue.get()
            # poses shape = (N, 15, 3) where N=people detected, 15=human body keypoints, 3=(x,y,z)
            line_sets = skeleton_poses(poses)
            vis.add_geometry(line_sets)


while True:
    start_time = time.time()
    vis.clear_geometries()
    vis.add_geometry(pcd)
    
    for person in people:
        indexed_pose = person[0]
        lineset = person[1]
        
        pose_data = indexed_pose[0]
        index = indexed_pose[1]
        
        points, lines = pose_lineset(pose_data)
        person[1].points = points
        person[1].lines = lines
        
        
        vis.add_geometry(person[1])
        vis.update_geometry(person[1])
        
        if (index + 1) >= skeleton_data.shape[0]:
            index = index - skeleton_data.shape[0]
            
        indexed_pose[1] = index + 1
        
        indexed_pose[0] = skeleton_data[index + 1]
        
        
    if keyboard.is_pressed('q'):
        vis.destroy_window()
        
    vis.poll_events()
    vis.update_renderer()
    
    print("FPS: ", 1.0 / (time.time() - start_time))


#o3d.visualization.draw_geometries(to_render)