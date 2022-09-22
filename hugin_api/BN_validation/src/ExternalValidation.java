import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import COM.hugin.HAPI.DefaultClassParseListener;
import COM.hugin.HAPI.DiscreteChanceNode;
import COM.hugin.HAPI.DiscreteNode;
import COM.hugin.HAPI.Domain;
import COM.hugin.HAPI.ExceptionHugin;
import COM.hugin.HAPI.Node;
import COM.hugin.HAPI.NodeList;
import COM.hugin.HAPI.NumberedDCNode;
import COM.hugin.HAPI.ParseListener;
import COM.hugin.HAPI.Table;

public class ExternalValidation {

	public static void main(String[] args) {
		String netName = "C:\\Users\\i.bermejo\\Documents\\Src\\bn-for-rt-plan-qa\\data29_1417_output_2";
		String data_folder = "C:\\Users\\i.bermejo\\Documents\\Data\\BN in RT\\20201123\\";
		String analysis = "2";
		String full_dataset = data_folder + "approach"+analysis+"finalCLEAN23112020_test.csv";
		String case_dataset = data_folder + "approach"+analysis+"finalCLEAN_stripped.dat";
		boolean calculateMarginals = false;
		boolean calculateProbabilityOfEvidence = false;
		boolean calculateProbabilityOfEachVariable = true;
		
        try {
        	System.out.println("Reading file: " + new Date(System.currentTimeMillis()));
	        ParseListener parseListener = new DefaultClassParseListener();
            Domain domain = new Domain (netName + ".net", parseListener);
            System.out.println("Finished reading file: " + new Date(System.currentTimeMillis()));
            domain.openLogFile (netName + ".log");
            domain.triangulate (Domain.H_TM_BEST_GREEDY);
            domain.compile();
            
            if(calculateMarginals)
            {
            
	            NodeList nodes = domain.getNodes();
	            for(Object node : nodes)
	            {
	            	NodeList justTheNode = new NodeList();
	            	justTheNode.add(node);
	            	Table marginal = domain.getMarginal( justTheNode);
	            	double[] marginalProbabilities = marginal.getData();
	            	DiscreteNode discreteNode = ((DiscreteNode)node);
	            	long nStates = discreteNode.getNumberOfStates();
	            	System.out.print("States for " + discreteNode.getName() + ":");
	            	for(long i=0; i<nStates; ++i)
	            	{
	            		if(discreteNode instanceof NumberedDCNode)
	            		{
	            			System.out.print(((NumberedDCNode)discreteNode).getStateValue(i) + ", ");
	            		}else
	            		{
	            			System.out.print(discreteNode.getStateLabel(i) + ", ");
	            		}
	            	}
	            	System.out.println();
	            	System.out.println("Marginal probabilities for " + discreteNode.getName() + ":" + Arrays.toString(marginalProbabilities));
	            }
            }
            
            if(calculateProbabilityOfEvidence)
            {
            	try {
                    FileWriter writer = new FileWriter("Output" + System.currentTimeMillis() + ".csv", true);
		            domain.parseCases (case_dataset, parseListener);
		            System.out.println("Finished loading cases: " + new Date(System.currentTimeMillis()));
		            for(int i = 0; i < domain.getNumberOfCases(); ++i)
		            {
		            	try {
			            	domain.enterCase(i);
			            	domain.propagate (Domain.H_EQUILIBRIUM_SUM, Domain.H_EVIDENCE_MODE_NORMAL);
			            	writer.write (i +"," + domain.getNormalizationConstant() + "\n");
		            	} catch (ExceptionHugin e) {
			                System.out.println ("Exception at case " + i + ": " + e.getMessage());
			                writer.write (i +", 0\n");
		            	}
		            }
                    writer.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }

	            System.out.println("Finished processing cases:" + new Date(System.currentTimeMillis()));
            }
            
            if(calculateProbabilityOfEachVariable)
            {
            	try {
            		BufferedReader br = new BufferedReader(new FileReader(full_dataset));
                    FileWriter writer = new FileWriter("Output_" + analysis + "_" + System.currentTimeMillis() + ".csv", true);
		            domain.parseCases (case_dataset, parseListener);
		            System.out.println("Finished loading cases: " + new Date(System.currentTimeMillis()));
		            List<String> headers = Arrays.asList(br.readLine().split(";"));
		            int numberOfNodes = 0;
		            NodeList nodes = domain.getNodes();

		            // Headers
		            writer.write ("Researchnumber,PBD,");
		            for(Object nodeObject : nodes)
		            {
			            Node node = (Node)nodeObject;
	            		String nodeName = node.getName();
	            		if(!nodeName.equals("Anatomic_tumor_loc") &&
	            				!nodeName.equals("T_Stage") && 
	            				!nodeName.equals("N_Stage") && 
	            				!nodeName.equals("M_Stage") && 
	            				!nodeName.equals("Treatment_Intent") &&
	            				headers.indexOf(nodeName) > -1)
	            		{
	            			writer.write (nodeName + ",");
	            			numberOfNodes++;
	            		}
		            }
		            System.out.println("Number of nodes: " + numberOfNodes);
		            writer.write ("incompatible_evidence,rt_error");
            		writer.write ("\n");
		            
		            for(int i = 0; i < domain.getNumberOfCases(); ++i)
		            {
		            	String line = br.readLine();
		                String[] lineItems = line.split(";");
		            	try {
			            	// Researchnumber
			            	writer.write (lineItems[0] + ",");
			            	
			            	// PBD
			            	writer.write (lineItems[1] + ",");

		            		domain.enterCase(i);
			            	domain.propagate (Domain.H_EQUILIBRIUM_SUM, Domain.H_EVIDENCE_MODE_NORMAL);
			            	
			            	for(Object nodeObject : nodes)
				            {
			            		Node node = (Node)nodeObject;
			            		String nodeName = node.getName();
			            		if(!nodeName.equals("Anatomic_tumor_loc") &&
			            				!nodeName.equals("T_Stage") && 
			            				!nodeName.equals("N_Stage") && 
			            				!nodeName.equals("M_Stage") && 
			            				!nodeName.equals("Treatment_Intent"))
			            		{
			            			String nodeValue = "";
			            			if(headers.indexOf(nodeName) > -1)
			            			{
			            				nodeValue = lineItems[headers.indexOf(nodeName)].replace("\"","");
				            				
				            			NodeList justTheNode = new NodeList();
				    	            	justTheNode.add(node);
				    	            	Table marginal = domain.getMarginal( justTheNode);
				    	            	double[] marginalProbabilities = marginal.getData();
				    	            	long stateIndex = -1;
				    	            	if(node instanceof NumberedDCNode)
				            			{
				    	            		try
				    	            		{
				    	            			stateIndex = ((NumberedDCNode)node).getStateIndex(Double.parseDouble(nodeValue));
				    	            		}catch(NumberFormatException e)
				    	            		{
				    	            			stateIndex = ((NumberedDCNode)node).getStateIndex(nodeValue);
				    	            		}
				            			}else
				            			{
				            				stateIndex = ((DiscreteChanceNode)node).getStateIndex(nodeValue);
				            			}
				    	            	if(stateIndex < 0)
				    	            	{
				    	            		System.out.println(i+"th case: State not found: node=" + nodeName + "; state= " + nodeValue);
				    	            		writer.write ("NaN,");
				    	            	}
				    	            	else
				    	            	{
				    	            		double probability = marginalProbabilities[(int)stateIndex];
				    	            		writer.write (probability + ",");
				    	            	}
			            			}
			            		}
				            }
			            	writer.write("0,");
			            	writer.write(lineItems[headers.indexOf("rt_error")]);
	    	            	writer.write ("\n");
			            	
		            	} catch (ExceptionHugin e) {
			                System.out.println ("Exception at case " + i + ": " + e.getMessage());
			                for(int j=0; j<numberOfNodes; ++j)
			                	writer.write ("NaN,");
			                writer.write("1,");
			                writer.write(lineItems[headers.indexOf("rt_error")]);
			                writer.write ("\n");
		            	}
		            }
		            br.close();
                    writer.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }

	            System.out.println("Finished processing cases:" + new Date(System.currentTimeMillis()));
            }
            domain.closeLogFile();
            domain.delete();
        
        } catch (Exception e) {
            System.out.println ("General exception: " + e.getMessage());
	    e.printStackTrace();
        }
    }

}
