Welcome to the NSIDC DataViewer program, version 1.1, written by David Fanning for the National Snow and Ice Data Center (NSIDC). The DataViewer was released 11 November 2008 and updated 4 August 2010.  The program is written in the Interactive Data Language (IDL) and works with IDL 6.4 and above. 

For information about IDL, visit the ITT Visual Information Solutions site at http://www.ittvis.com.

The DataViewer works with the following NSIDC data sets, listed by data set title and URL:
	DMSPSSM/I-SSMIS Daily Polar Gridded Brightness Temperatures 
		http://nsidc.org/data/nsidc-0001.html
  	DMSP SSM/I-SSMIS Pathfinder Daily EASE-Grid Brightness Temperatures 
		http://nsidc.org/data/nsidc-0032.html
	Northern Hemisphere EASE-Grid Weekly Snow Cover and Sea Ice Extent Version 3 
		http://nsidc.org/data/nsidc-0046.html
   	Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data 
		http://nsidc.org/data/nsidc-0051.html
	Nimbus-7 SMMR Pathfinder Daily EASE-Grid Brightness Temperatures 
		http://nsidc.org/data/nsidc-0071.html
	Bootstrap Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS 
		http://nsidc.org/data/nsidc-0079.html
	Near-Real-Time DMSP SSM/I-SSMIS Gridded Brightness Temperatures
		http://nsidc.org/data/nsidc-0080.html
   	Near-Real-Time DMSP SSM/I-SSMIS Daily Polar Gridded Sea Ice Concentrations 
		http://nsidc.org/data/nsidc-0081.html
	Polar Pathfinder Daily 25km EASE-Grid Sea Ice Motion Vectors* 
		http://nsidc.org/data/nsidc-0116.html
		* Note: The ice motion vectors only display the mean gridded fields.
   	AMSR-E/Aqua Daily EASE-Grid Brightness Temperatures 
		http://nsidc.org/data/nsidc-0301.html
   	AMSR-E/Aqua Daily Global Quarter-Degree Gridded Brightness Temperatures 
		http://nsidc.org/data/nsidc-0302.html
	Near-Real-Time SSM/I-SSMIS Pathfinder Daily Gridded Brightness Temperatures 
		http://nsidc.org/data/nsidc-0342.html
   	AMSR-E/Aqua Daily L3 25 km Brightness Temperature & Sea Ice Concentration Polar Grids 
		http://nsidc.org/data/ae_si25.html

Additionally, the program reads BMP, GIF, JPEG, PNG, PPM, SRF, TIFF, DICOM, or JPEG2000 image files.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET-UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IDL is required for the use of the DataViewer. If you do not have IDL, you may download the free IDL Virtual Machine (VM) from ITTVIS at http://www.ittvis.com. NSIDC distributes the dataviewer.sav file, which contains all the programs and libraries needed to run the DataViewer program. The dataviewer.sav file can be used in either IDL or the IDL VM.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DOWNLOADING IDL VIRTUAL MACHINE (VM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

If you do not have an IDL license, you may download the IDL VM from ITTVIS at http://www.ittvis.com. If you have an IDL license, these steps can be skipped.

Step 1: Register to become a user of the IDL VM.  The ITTVIS Web Administrator will send you an automated email with your verification code, and a link to submit your final approval.  

Step 2: It may take a full day to hear back with your approval, but there should be no problem with being approved.  Once you are approved, you can download the VM to your workstation.  If you do not hear from ITTVIS within one day, please contact them by phone.  The number is listed on their Web site at http://www.ittvis.com.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DATAVIEWER DIRECTORIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The following files, folders or directories are included with the DataViewer application and are located within the folder labeled "dataviewer": 

\config\
This folder contains configuration text files.

\resources\
This folder contains resources required for the DataViewer program.

\source\
This folder contains the IDL source code for the DataViewer program. The source folder will not be available if you have downloaded the DataViewer program from the NSIDC web page. Distributions without a source folder will have an DataViewer save file (see below) instead.

\dataviewer.sav file\ 
This is an IDL "save" file. This file is only present if you have not downloaded a DataViewer source code distribution. The dataviewer.sav file may be "restored" from within an IDL session, or it may be opened by the IDL Virtual Machine to run the DataViewer program. It is important, however, not to move the dataviewer.sav file with respect to the other files and folders in the dataviewer folder. If the dataviewr.sav file is moved in relation to the other files and folders, the DataViewer program will not be able to run correctly. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CONFIGURING THE PROGRAM 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

In order for the DataViewer to find the data you are working with, you will need to change the configuration file for the DataViewer application.  There are several ways you can do this.

Inside the config folder you will find the file “dataviewer_default.txt”. Open the configuration file in any text editor, and find the following line in the file: DATAVIEWER_DATA_DIRECTORY, 'DEFAULT'

Remove the word "DEFAULT" between the single quotes and substitute the exact location of your data directory. Be absolutely sure the entire directory path/name is enclosed in one pair of single quotes. 

You can also make the change from within the DataViewer. The first time you open the DataViewer application, if you have not changed the default location for the data directory (the location where your images are stored), it will assume your image files are located in a "data" directory in the dataviewer folder. This assumption will almost always be incorrect, and if the data directory does not exist, you will be prompted to select a different data directory. If you choose not to select a data directory at this time, the dataviewer folder itself will be the temporary data directory. Otherwise, select or create a directory of your choosing and this will become the data directory for this session. This directory you select or create will then automatically be saved as the new default location in the configuration file (dataviewer_default.txt).

You may change the default data directory at any time from within the DataViewer. To do so, select “Change Default Data Directory…” from the “File” menu. The directory you select will automatically be saved as the new default directory in your current configuration file.

Note that as you open files in the DataViewer, the DataViewer will always return to the last location
in which you selected an image file, regardless of the default data directory location.

Along with the data directory, you may also change other default settings, such as image layout, from within the DataViewer at any time. To do so, select “Edit Current Configuration File…” from the “File” menu. Make your changes, then select “Apply Configuration” to view your new settings. When you are satisfied with your changes, select “Save Configuration.”

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RUNNING DATAVIEWER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

If you have a valid IDL license, you can run the DataViewer save file as a run-time application. On a Windows computer, just double-click the dataviewer.sav program icon to start IDL as a run-time application. On a UNIX system, indicate that IDL should be started in run-time mode, like this, where the text "<path>" should be replaced by the actual path to the save file:

	% idl -rt=<path>/dataviewer.sav 

If you downloaded the IDL VM, you can run the dataviewer.sav file as an application. Using your mouse, drag the dataviewer.sav icon onto the VM icon, or just double click the VM icon and select the DataViewer save file in the resulting file dialog box.

If you have not added the DataViewer to your IDL path, you can run the program from within IDL by typing as command like this, where the text "<path>" should be replaced by the actual path to the save file:
	
	IDL>restore, <path>/dataviewer.sav

If you have added the DataViewer to your IDL path (see ADDING DATAVIEWER TO THE IDL PATH for instruction), you can run the program from within IDL by typing "dataviewer" at the IDL command line.

	IDL> dataviewer

If you like, you can pass the program the name of an image file to open:

	IDL> dataviewer, 'C:\data\EASE-F13-NL2006364D.85H.gz'

Or, you can pass it the name of a folder. The program will read and open all the image files in the folder.
	
	IDL> dataviewer, 'C:\data\'

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ADDING DATAVIEWER TO THE IDL PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

There are many ways to add a folder and its sub-folders to the IDL path (!PATH). See the IDL online help for additional information. One way to make sure all the DataViewer programs are on your IDL path is to enter IDL and type one or the other of the following two commands, depending upon your machine operating system. 
	
UNIX OPERATING SYSTEM
Assuming you have installed the dataviewer_source folder in your home directory inside the folder IDL, for example  ~/IDL/dataviewer/, you can type the following command  within IDL to add the dataviewer directory and all of its sub-directories to the IDL path:  
       
	IDL> !PATH = Expand_Path('+~/IDL/dataviewer/') + ':' + !PATH
       
WINDOWS OPERATING SYSTEM
Assuming you have installed the dataviewer folder on your C drive, inside the folder IDL (for example C:\IDL\dataviewer), you can type this command within IDL to add the dataviewer directory and all of its sub-directories to the IDL path:     
     
	IDL> !PATH = Expand_Path('+C:\IDL\dataviewer\') + ';' + !PATH.  
	(Note: This is not necessary if using the IDL VM).

Notice the plus sign (+) in front of the directory name for both operating systems. This is an indication to IDL that this directory should be expanded and all the sub-directories in this directory should also be added to the path. Note that in UNIX, the files in the path are separated by colons, whereas in WINDOWS, the files are separated by semi-colons. If you know how to do it, you can also include the appropriate command above in your IDL start-up file, so the DataViewer program is added to your IDL path whenever IDL is started.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
USING DATAVIEWER OPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The DataViewer provides different mechanisms for displaying data images. Tab options in the navigation bar allow you to control the image operation you would like to run, including image display, image animate, color change, etc. When images are displayed in the DataViewer, you can scroll over a particular image and make further changes just to that image. Use your right-click mouse button to bring up a pop up window with operations for that specific image.  

SELECTING FILES
You can open files in the DataViewer by choosing either the Open Image Files or the Open Image Directory of Files under the File tab. The latter button allows you to choose an image directory and the program attempts to open all the image files in the directory. If you use the Open Image Files button, you have the opportunity to filter files before selection. Multiple files can be selected by holding down the SHIFT or CONTROL keys while you make a selection.  Note that if a single file is selected, the window layout is changed to a 1-by-1 layout grid, regardless of the current window layout grid configuration.

CHANGING THE WINDOW LAYOUT
The DataViewer program appears in a completely resizeable window. Feel free to adjust the size of the window for optimal viewing of images. Simply grab any side or corner of the DataViewer window with your mouse and size accordingly. You can change the layout of the images at any time in the DataViewer window by selecting the Change Window Layout Grid button from the File tab. Up to 64 image files in an 8-by-8 grid layout is allowed.

SAVING THE DATAVIEWER DISPLAY WINDOW
The DataViewer display window can be saved at any time to a variety of file formats. Choose the Save Window As button from the File tab to save the window in JPEG, PNG, PostScript, and other file formats.

CHANGING COLORS
Colors for the image, and for different properties in the image (missing values, out-of-bounds values, etc.), can be changed from the Colors tab.

ANIMATING IMAGES
All selected images can be animated by selecting the Animate All Images button from the Operations tab.

STRETCHING OR SCALING IMAGES
All selected images can be stretched with Histogram stretching or rescaled by selecting the Histogram Stretch All Images button from the Operations tab. Various stretches, including LINEAR, LOG, and GAMMA stretches, can be applied.

REFRESHING IMAGES
All images can be refreshed and returned to their default properties by choosing the Refresh All Images button from the Operations tab.

DISPLAYING IMAGE NAMES
Typically, images are displayed with their names below the images. This functionality can be turned on or off by selecting the Image Names button from the Operations tab.

DISPLAYING IMAGE COLOR BARS
Typically, images are displayed with a color bar above the images. This functionality can be turned on or off by selecting the Colorbars button from the Operations tab.

REARRANGING IMAGES
Images are read into the program in alphabetical order. You can rearrange the images in the current display window by left-clicking inside the image and dragging it on top of the image location where you would like the image to be. All other images in the window will move to accommodate this rearrangement. 

IMAGE INFORMATION
As you move your mouse over the images in the DataViewer display, you see the name of the image, its value, and pixel location (longitude, latitude) displayed in the status bar widget of the program interface. In this way, you can see the value of the image under your cursor.

OTHER IMAGE PROPERTIES
You can change individual properties of images by right-clicking on individual images. Properties will include changing image colors, scaling the image, displaying the image in its natural size, and annotating the image (see below), among various other properties.

ANNOTATING THE IMAGE
You can annotate any image you like by right-clicking on the image you wish to annotate and choosing the Annotate Image selection from the pop-up tab. If you choose to add text to the annotation, be sure to hit the carriage return when you are finished typing text; this will ensure the text is "set" in the annotation window. You can select and drag annotations in the window to position them where you want them. If you select an annotation and then right-click it, you will be able to change the properties of that annotation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ASSISTANCE WITH THE DATAVIEWER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

If you need help with this program or its set-up and you downloaded the program from the
NSIDC web page, please contact NSIDC User Services:

National Snow and Ice Data Center (NSIDC) User Services
NSIDC/CIRES, 449 UCB
University of Colorado
Boulder, Colorado 80309-0449 USA
Phone: +1 303.492.6199
Email: nsidc@nsidc.org

If you need help with the source code version of the software, please contact the author:

FANNING SOFTWARE CONSULTING
David Fanning, Ph.D.
1645 Sheely Drive
Fort Collins, CO 80526 USA
Phone: 970-221-0438
E-mail: davidf@dfanning.com
Coyote's Guide to IDL Programming: http://www.dfanning.com
