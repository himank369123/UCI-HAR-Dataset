
The data originally contained a large list of features with documentation in the 'features_info.txt' file found in this repository.
The data comes from measurements of accelerometer data and gyroscope data, both of which measure accelerations.
For our analysis, we only retain measurements which calculated a mean or standard deviation of either an acceleration or a jerk (derivative of acceleration).

Some measurements are the result of a Fast Fourier Transform on the data to make a frequency domain measurement, which is filtered to reduce noise - details on these measurements can be found in the 'features_info.txt' file.
Accelerometer data is often broken down into body and gravitational acceleration to filter out the effect of gravity on e.g. walking up or down stairs.
All data in the original data set are normalized on a [-1,1] scale, which makes them unitless and directly comparable.

## Code Book
Here we list each variable we store with a short explanation of its contents.
All variables have remained normalized on a [-1,1] scale, and so have no units.

### bodyaccmean
A mean of measurements of the accelerometer reading body movements.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### gravityaccmean
A mean of measurements of the acceleromter registering gravitational acceleration.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodyaccjerkmean
A mean of measurements of the jerk, the change in acceleration according to the acceleromter on the body.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodygyromean
A mean of measurements of the gyroscope. Represents rotational acceleration.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

###bodygyrojerkmean
A mean of measurements of the jerk, the change in acceleration, according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodyaccmean
A mean of measurements in the frequency domain of the acceleration of the body.
These contain both the standard measurements and the measurements of the frequency (meanfreq instead of mean).
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.


### freqbodyaccjerkmean
A mean of measurements of the jerk, or change in acceleration, in the frequency domain, due to the body.
These contain both the standard measurements and the measurements of the frequency (meanfreq instead of mean).
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodygyromean
A mean of measurements of the acceleration according to the gyroscope, calculated in the frequency domain.
These contain both the standard measurements and the measurements of the frequency (meanfreq instead of mean).
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodyaccstddeviation
The standard deviation of acceleration measurements according to the accelerometer, due to the body.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### gravityaccstddeviation
The standard deviation of acceleration measurements due to gravity according to the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodyaccjerkstddeviation
The standard deviation of jerk measurements, or changes in acceleration, due to the body, according to the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodygyrostddeviation
The standard deviation of acceleration of the body according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodygyrojerkstddeviation
The standard deviation of jerk, or change in acceleration, measurements of the body according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodyaccstddeviation
The standard deviation of acceleration measurements in the frequency domain, according to the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodyaccjerkstddeviation
The standard deviation of jerk, or change in acceleration, of the body according to frequency domain measurements of the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodygyrostddeviation
The standard deviation of the acceleration in the frequency domain according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.





## Code Book
Here we list each variable we store with a short explanation of its contents.
All variables have been normalized on a [-1,1] scale in the original data.

### bodyaccmean
A mean of measurements of the accelerometer reading body movements.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### gravityaccmean
A mean of measurements of the acceleromter registering gravitational acceleration.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodyaccjerkmean
A mean of measurements of the jerk, the change in acceleration according to the acceleromter on the body.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodygyromean
A mean of measurements of the gyroscope. Represents rotational acceleration.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

###bodygyrojerkmean
A mean of measurements of the jerk, the change in acceleration, according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodyaccmean
A mean of measurements in the frequency domain of the acceleration of the body.
These contain both the standard measurements and the measurements of the frequency (meanfreq instead of mean).
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.


### freqbodyaccjerkmean
A mean of measurements of the jerk, or change in acceleration, in the frequency domain, due to the body.
These contain both the standard measurements and the measurements of the frequency (meanfreq instead of mean).
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodygyromean
A mean of measurements of the acceleration according to the gyroscope, calculated in the frequency domain.
These contain both the standard measurements and the measurements of the frequency (meanfreq instead of mean).
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodyaccstddeviation
The standard deviation of acceleration measurements according to the accelerometer, due to the body.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### gravityaccstddeviation
The standard deviation of acceleration measurements due to gravity according to the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodyaccjerkstddeviation
The standard deviation of jerk measurements, or changes in acceleration, due to the body, according to the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodygyrostddeviation
The standard deviation of acceleration of the body according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### bodygyrojerkstddeviation
The standard deviation of jerk, or change in acceleration, measurements of the body according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodyaccstddeviation
The standard deviation of acceleration measurements in the frequency domain, according to the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodyaccjerkstddeviation
The standard deviation of jerk, or change in acceleration, of the body according to frequency domain measurements of the accelerometer.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.

### freqbodygyrostddeviation
The standard deviation of the acceleration in the frequency domain according to the gyroscope.
Contributes an 'x', 'y', and 'z' measurement, representing three directions, and a 'mag' variable taking the magnitude.