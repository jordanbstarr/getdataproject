#Codebook

##Data
The data for this analysis is from experiments carried out on a group of 30 volunteers. Each person performed six activities while wearing a Samsung Galaxy S II on his or her waist: walking, walking upstairs, walking downstairs, sitting, standing, and laying. Using the phone's accelerometer and gyroscope, the experiment captured 3-axial linear acceleration and 3-axial angular velocity data. The initial dataset was randomly partitioned into two sets, where 70% of the volunteers were selected for generating the training data and the rest generated the test data.

##Transformations
1. The data analyzed was initially stored in six separate txt files.  As described in the R script, all six datasets were merged to create a complete dataset.
2. At this point, the data had 563 variables, but we were only interested in the variables that provided mean or standard deviation values.  Therefore, I subsetted the data for those columns.
3. To ease interpretation of the activities being performed, which were initially coded from one to six, I transformed each coded activity integer to its value in characters.
4. The variables were still unlabeled at this juncture, so next I provided a descriptive variable name for each column.  This will be further elaborated upon in the "Variables" section.
5. Lastly, I grouped the data by subject (1-30) and activity type, and then I found the mean of each variable within those groups.  This summarised data is then returned by the R script as a txt file in the working directory.

##Variables

####First Letter
* t = time-domain acceleration measurement (m/s^2)
* f = frequency-domain acceleration measurement derived with Fast Fourier Transformation (Hz)

####Words
* body = body acceleration measurement
* gravity = gravity acceleration measurement
* acc = linear accelerometer measurement
* gyro = gyroscopic accelerometer measurement
* jerk = jerk signal (3-dimensional signal derived from body linear & gyroscopic measurements)
* mag = magnitude value (calculated with euclidean norm)
* mean = mean value
* std = standard deviation 

####Last Letter
* x = Indicates acceleration in the x-plane
* y = Indicates acceleration in the y-plane
* z = Indicates acceleration in the z-plane



