import os.path
import os
# montagepath = os.path.join(os.path.expanduser("~"),"dev","HCP","Analysis","Cognition","Script","brainmontageplot")
# print(os.listdir(montagepath))
# os.chdir(montagepath)
print(os.listdir())
print(os.getcwd())

# import hello
from brainmontageplot import create_montage_figure

import numpy as np




dir_analysis = os.path.join(os.path.expanduser("~"),"dev","HCP","Analysis")

dir_output = os.path.join(dir_analysis,"Sex/output")
dir_temp = os.path.join(dir_analysis,"Sex/temp")

variable = "family_split"
splitBy = "Parent"
# xvars <- c("mean", "sd","pacf", "acf","yycor")
xvars = ["mean", "sd", "acf", "pacf"]

mix = "0.5" # 1 for lasso, 0 for ridge
splitratio = "0.8" # train/test
x_var_name = "_".join(xvars)

folder_name = "{}/splitBy_{}/elastic/split_{}/mix_{}/{}".format(variable, splitBy, splitratio, mix, x_var_name)

mean_file_name = os.path.join(dir_temp, folder_name, "abs.csv" )
mean_abs = np.loadtxt(mean_file_name, delimiter=",", skiprows=0)

print(type(mean_abs))

output_folder = os.path.join(dir_output, x_var_name)

if not os.path.exists(output_folder):
    os.makedirs(output_folder)

if ("mean" in xvars):
    mean_array = mean_abs[0:86]
    print(mean_array)
    img=create_montage_figure(mean_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma')
    from PIL import Image
    mean_output = os.path.join(output_folder, "mean.png")
    Image.fromarray(img).save(mean_output)
    mean_abs = mean_abs[86:]

if ("sd" in xvars):
    sd_array = mean_abs[0:86]
    print(sd_array)
    img=create_montage_figure(sd_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma')
    from PIL import Image
    sd_output = os.path.join(output_folder, "sd.png")
    Image.fromarray(img).save(sd_output)
    mean_abs = mean_abs[86:]

if ("pacf" in xvars):
    pacf1_array = mean_abs[0:86]
    print(pacf1_array)
    img=create_montage_figure(pacf1_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma', clim=[min(pacf1_array), max(pacf1_array)])
    from PIL import Image
    pacf1_output = os.path.join(output_folder, "pacf1.png")
    Image.fromarray(img).save(pacf1_output)
    mean_abs = mean_abs[86:]

    pacf2_array = mean_abs[0:86]
    print(pacf2_array)
    img=create_montage_figure(pacf2_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma', clim=[min(pacf2_array), max(pacf2_array)])
    from PIL import Image
    pacf2_output = os.path.join(output_folder, "pacf2.png")
    Image.fromarray(img).save(pacf2_output)
    mean_abs = mean_abs[86:]

    pacf3_array = mean_abs[0:86]
    print(pacf3_array)
    img=create_montage_figure(pacf3_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma', clim=[min(pacf3_array), max(pacf3_array)])
    from PIL import Image
    pacf3_output = os.path.join(output_folder, "pacf3.png")
    Image.fromarray(img).save(pacf3_output)
    mean_abs = mean_abs[86:]

if ("acf" in xvars):
    acf1_array = mean_abs[0:86]
    print(acf1_array)
    img=create_montage_figure(acf1_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma', clim=[min(acf1_array), max(acf1_array)])
    from PIL import Image
    acf1_output = os.path.join(output_folder, "acf2.png")
    Image.fromarray(img).save(acf1_output)
    mean_abs = mean_abs[86:]

    acf2_array = mean_abs[0:86]
    print(acf2_array)
    img=create_montage_figure(acf2_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma', clim=[min(acf2_array), max(acf2_array)])
    from PIL import Image
    acf2_output = os.path.join(output_folder, "acf3.png")
    Image.fromarray(img).save(acf2_output)
    mean_abs = mean_abs[86:]

    acf3_array = mean_abs[0:86]
    print(acf3_array)
    img=create_montage_figure(acf3_array,atlasname='fs86',
    viewnames='all',surftype='infl',colormap='magma', clim=[min(acf3_array), max(acf3_array)])
    from PIL import Image
    acf3_output = os.path.join(output_folder, "acf4.png")
    Image.fromarray(img).save(acf3_output)
    mean_abs = mean_abs[86:]

# roivals= #example values for each ROI

# img=create_montage_figure(roivals,atlasname='fs86',
#     viewnames='all',surftype='infl',clim=[0,86],colormap='magma')

# from PIL import Image
# Image.fromarray(img).save('mydata_montage.png')