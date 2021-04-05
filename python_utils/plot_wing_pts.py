import sys
import h5py
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import animation


class WingPlot3d:

    def __init__(self):

        self.xyz_to_yzx = True
        self.elem_step = 4

        filename = sys.argv[1] 
        h5f= h5py.File(filename,'r')
        self.fig_ax_lim = 0.003
        
        self.ax_array = np.array(h5f['ax_array'])
        self.le_array = np.array(h5f['le_array'])
        self.te_array = np.array(h5f['te_array'])
        
        _, self.num_elem, self.num_t = self.ax_array.shape
        
        self.fig = plt.figure()
        self.fig_ax = self.fig.gca(projection='3d')
        
        self.t_ind = 0
        self.le_line_dict = {}
        self.te_line_dict = {}
        for elem_ind in range(0,self.num_elem,self.elem_step):
        
            x,y,z = self.get_le_xyz(elem_ind, self.t_ind)
            line, = self.fig_ax.plot(x, y, z, 'b')
            self.le_line_dict[elem_ind] = line
        
            x,y,z = self.get_te_xyz(elem_ind, self.t_ind)
            line, = self.fig_ax.plot(x, y, z, 'b')
            self.te_line_dict[elem_ind] = line

        if self.xyz_to_yzx:
            self.fig_ax.set_xlabel('z')
            self.fig_ax.set_ylabel('x')
            self.fig_ax.set_zlabel('y')
        else:
            self.fig_ax.set_xlabel('x')
            self.fig_ax.set_ylabel('y')
            self.fig_ax.set_zlabel('z')

        self.fig_ax.set_xlim(-self.fig_ax_lim, self.fig_ax_lim)
        self.fig_ax.set_ylim(-self.fig_ax_lim, self.fig_ax_lim)
        self.fig_ax.set_zlim(-self.fig_ax_lim, self.fig_ax_lim)

    def run(self):
        print('run')
        self.animation = animation.FuncAnimation(self.fig, self.update, interval=0.001)
        plt.show()

    def update(self,dummy):
        self.t_ind += 1
        if self.t_ind >= self.num_t:
            self.t_ind = 0
        print(self.t_ind)

        for elem_ind in range(0,self.num_elem,self.elem_step):
            x,y,z = self.get_le_xyz(elem_ind, self.t_ind)
            le_line = self.le_line_dict[elem_ind]
            le_line.set_data_3d(x,y,z)

            x,y,z = self.get_te_xyz(elem_ind, self.t_ind)
            te_line = self.te_line_dict[elem_ind]
            te_line.set_data_3d(x,y,z)


    def get_le_xyz(self, elem_ind, t_ind): 
        ax_vect = self.ax_array[:, elem_ind, t_ind]
        le_vect = self.le_array[:, elem_ind, t_ind]
        te_vect = self.te_array[:, elem_ind, t_ind]
        if self.xyz_to_yzx:
            y = [ax_vect[0], le_vect[0]]
            z = [ax_vect[1], le_vect[1]]
            x = [ax_vect[2], le_vect[2]]
        else:
            x = [ax_vect[0], le_vect[0]]
            y = [ax_vect[1], le_vect[1]]
            z = [ax_vect[2], le_vect[2]]
        return x,y,z

    def get_te_xyz(self, elem_ind, t_ind): 
        ax_vect = self.ax_array[:, elem_ind, t_ind]
        le_vect = self.le_array[:, elem_ind, t_ind]
        te_vect = self.te_array[:, elem_ind, t_ind]
        if self.xyz_to_yzx:
            y = [ax_vect[0], te_vect[0]]
            z = [ax_vect[1], te_vect[1]]
            x = [ax_vect[2], te_vect[2]]
            pass
        else:
            x = [ax_vect[0], te_vect[0]]
            y = [ax_vect[1], te_vect[1]]
            z = [ax_vect[2], te_vect[2]]
        return x,y,z

# ---------------------------------------------------------------------------------------
if __name__ == '__main__':

    wing_plot = WingPlot3d()
    wing_plot.run()






