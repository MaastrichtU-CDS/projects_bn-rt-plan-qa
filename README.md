# Bayesian network for error detection for radiotherapy planning
Bayesian network for error detection for radiotherapy planning

1. Preprocessing

DataPreprocessing_Multiparty.R file

Make sure to change the source_folder to point at the right folder in your computer. It should contain an excel file with the data and the binning file.

Check there are no errors in the data with the check_binning function and fix any identified errors. The function reads the binning from an excel file. 

Generate the files for the Bayesian network training and testing with Hugin.

Manually remove the quotes from the first row in the files.

2. Learning

Launch Hugin. Open any .net files with the correct structure (ideally the last one).

Go to the menu Wizards > EM Learning Wizard... 

Pick the right file  with the data to train and hit next.

It is generally a good idea to Preview the data in the next screen and then hit next.

In this screen, we choose whether learn the probabilities from scratch (in this case, click on the button All of the Reset group both in the Distributions and Experience tabs) or fine tune the existing probabilities (next).

Then click on 'Perform EM learning' and wait for a long time until you see a pop up. Save the .net file and close Hugin.

3. Result generation

First, open Eclipse. Eclipse is a Java IDE and can be downloaded for free at eclipse.org. The github repo contains an Eclipse project within the BN_validation folder.

To load it into Eclipse, go to File > Import... > General > Existing projects into workspace, then navigate to the BN validation folder.

Linking Hugin with Eclipse: On the Project Explorer tab (usually on the left), you should see the BN_validation project folder. Under Referenced Libraries, you should see hapi74-64.jar. 

If that is not the case, right click on the BN_validation folder > Properties > Java Build Path > Libraries tab > Add external JARs > navigate to C:\Program Files\Hugin Expert\Hugin Researcher 7.4\HDE7.4J\Lib and select hapi74-64.jar.

Open the InternalExternalValidation.java or PoznanValidation.java and check that the variables pointing at folders and BN files are correct. I use flags to determine which parts of the code to run, e.g. which networks to use as training and which for testing. NB: 'complete info' stands for 'using info from all variables not only the initial 5'.

Then go to menu Run > Run as > Java application (Accept whatever comes next) and the process will run.

This will create Output_validation_xxxxx.csv files specifying training and testing files with a timestamp.

Those will be used in the next step in R

3. Result processing

Done in R using ResultProcessing.R

It compares the Output_validation_xxxxx files with the testing csv file generated in the first step and calculates AUCs and ROC plots.
 

