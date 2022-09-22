import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import COM.hugin.HAPI.DefaultClassParseListener;
import COM.hugin.HAPI.DiscreteChanceNode;
import COM.hugin.HAPI.DiscreteNode;
import COM.hugin.HAPI.Domain;
import COM.hugin.HAPI.ExceptionHugin;
import COM.hugin.HAPI.ExceptionParse;
import COM.hugin.HAPI.Node;
import COM.hugin.HAPI.NodeList;
import COM.hugin.HAPI.NumberedDCNode;
import COM.hugin.HAPI.ParseListener;
import COM.hugin.HAPI.Table;

public class InternalExternalValidation {
	
	private static boolean calculateMarginals = false;
	private static boolean calculateProbabilityOfEvidence = false;
	private static boolean calculateProbabilityOfEachVariable = true;
	private static boolean calculateProbabilityOfEachVariableCompleteInfo = false;
	private static boolean crossSiteTesting = false;
	private static boolean combinedSiteTesting = false;
	private static boolean allSiteTesting = true;

	public static void main(String[] args) {
		// String training_site = "UW";
		// String testing_site = "UVM";
		String[] sites = new String[] {"Maastro", "UVM", "UW"};
		
		if(crossSiteTesting)
		{
			for(String training_site : sites)
			{
				for(String testing_site : sites)
				{
					System.out.println("Testing on " + testing_site + " data using model trained on " + training_site);
					test(training_site, testing_site);
				}
			}
		}
		
		if(combinedSiteTesting)
		{
			test("Maastro_UW", "UVM");
			test("Maastro_UVM", "UW");
			test("UVM_UW", "Maastro");
		}
		if(allSiteTesting)
		{
			test("All", "All");
		}
		
	}
	
	public static void test(String training_site, String testing_site)
	{
		// Paths
		String homedir = "C:\\Users\\i.bermejo\\Documents";
		String data_folder = homedir + "\\Data\\BN in RT\\";
		String tmp_folder = data_folder + "tmp\\";
		String net_folder = homedir + "\\Src\\bn-for-rt-plan-qa\\";
		// Net name
		String netNameMaastro =     net_folder + "RT_Plan_Error_detection_20220908_Maastro"; //data29_1417_output_2";
		String netNameUVM =         net_folder + "RT_Plan_Error_detection_20220908_UVM"; //data29_1417_output_2";
		String netNameUW =          net_folder + "RT_Plan_Error_detection_20220908_UW";
		String netNameMaastro_UW =  net_folder + "RT_Plan_Error_detection_20220908_Maastro_UW";
		String netNameMaastro_UVM = net_folder + "RT_Plan_Error_detection_20220908_Maastro_UVM";
		String netNameUVM_UW =      net_folder + "RT_Plan_Error_detection_20220908_UVM_UW";
		String netNameAll =         net_folder + "RT_Plan_Error_detection_20220908_All";
		// Test dataset
		String full_datasetMaastro = data_folder + "Hugin-BN-RT-QA-MAASTRO_test.csv";
		String full_datasetUVM =     data_folder + "UVM_1821_binned_error_v2.csv";
		String full_datasetUW =      data_folder + "UWSCCA_1821_binned_error_v2.csv";
		String full_datasetAll =     data_folder + "BN-RT-QA-all-test.csv";
		// Test case datasets
		String case_datasetMaastro = data_folder + "Hugin-BN-RT-QA-MAASTRO_test.dat";
		String case_datasetUVM =     data_folder + "Hugin-BN-RT-QA-UVM_test.dat";
		String case_datasetUW =      data_folder + "Hugin-BN-RT-QA-UW_test.dat";
		String case_datasetAll =     data_folder + "Hugin-BN-RT-QA-All_test.dat";

		String netName = "";
		String full_dataset = "";
		String case_dataset = "";

		switch(training_site)
		{
			case "Maastro": 
				netName = netNameMaastro;
				break;
			case "UVM": 
				netName = netNameUVM;
				break;
			case "UW": 
				netName = netNameUW;
				break;
			case "Maastro_UW": 
				netName = netNameMaastro_UW;
				break;
			case "Maastro_UVM": 
				netName = netNameMaastro_UVM;
				break;
			case "UVM_UW": 
				netName = netNameUVM_UW;
				break;
			case "All": 
				netName = netNameAll;
				break;
		}

		switch(testing_site)
		{
			case "Maastro": 
				full_dataset = full_datasetMaastro;
				case_dataset = case_datasetMaastro;
				break;
			case "UVM": 
				full_dataset = full_datasetUVM;
				case_dataset = case_datasetUVM;
				break;
			case "UW": 
				full_dataset = full_datasetUW;
				case_dataset = case_datasetUW;
				break;
			case "All": 
				full_dataset = full_datasetAll;
				case_dataset = case_datasetAll;
				break;
		}
				
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
                    FileWriter writer = new FileWriter(data_folder + "\\Output_validation_" + training_site + "_" + testing_site + "_" + new SimpleDateFormat("yyyyMMddHHmmss").format(new Date(System.currentTimeMillis())) + ".csv", true);
		            domain.parseCases (case_dataset, parseListener);
		            System.out.println("Finished loading cases: " + new Date(System.currentTimeMillis()));
		            String header = br.readLine();
		            header = header.replaceAll("\"","");
		            List<String> variableNames = Arrays.asList(header.split(","));
		            int numberOfNodes = 0;
		            NodeList nodes = domain.getNodes();

		            // Headers
		            writer.write ("Researchnumber,");
		            //writer.write ("Researchnumber,PBD,");
		            for(Object nodeObject : nodes)
		            {
			            Node node = (Node)nodeObject;
	            		String nodeName = node.getName();
	            		if(!nodeName.equals("Anatomic_Tumor_Location") &&
	            				!nodeName.equals("T_Stage") && 
	            				!nodeName.equals("N_Stage") && 
	            				!nodeName.equals("M_Stage") && 
	            				!nodeName.equals("Treatment_Intent") &&
	            				variableNames.indexOf(nodeName) > -1)
	            		{
	            			writer.write (nodeName + ",");
	            			numberOfNodes++;
	            		}
		            }
		            System.out.println("Number of nodes: " + numberOfNodes);
		            writer.write ("incompatible_evidence,errors");
            		writer.write ("\n");
		            
		            for(int i = 0; i < domain.getNumberOfCases(); ++i)
		            {
		            	String line = br.readLine();
		                String[] lineItems = line.split(",");
		            	try {
			            	// Researchnumber
			            	writer.write (lineItems[0] + ",");
			            	
			            	// PBD
			            	//writer.write (lineItems[1] + ",");

		            		domain.enterCase(i);
			            	domain.propagate (Domain.H_EQUILIBRIUM_SUM, Domain.H_EVIDENCE_MODE_NORMAL);
			            	
			            	for(Object nodeObject : nodes)
				            {
			            		Node node = (Node)nodeObject;
			            		String nodeName = node.getName();
			            		if(!nodeName.equals("Anatomic_Tumor_Location") &&
			            				!nodeName.equals("T_Stage") && 
			            				!nodeName.equals("N_Stage") && 
			            				!nodeName.equals("M_Stage") && 
			            				!nodeName.equals("Treatment_Intent"))
			            		{
			            			String nodeValue = "";
			            			if(variableNames.indexOf(nodeName) > -1)
			            			{
			            				nodeValue = lineItems[variableNames.indexOf(nodeName)].replace("\"","");
				            				
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
				    	            		if(!"NULL".equals(nodeValue))
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
			            	writer.write(lineItems[variableNames.indexOf("errors")]);
	    	            	writer.write ("\n");
			            	
		            	} catch (ExceptionHugin e) {
			                System.out.println ("Exception at case " + i + ": " + e.getMessage());
			                for(int j=0; j<numberOfNodes; ++j)
			                	writer.write ("NaN,");
			                writer.write("1,");
			                writer.write(lineItems[variableNames.indexOf("errors")]);
			                writer.write ("\n");
		            	}
		            }
		            br.close();
                    writer.close();
            	} catch (ExceptionParse e1) {
                    System.out.println("Exception thrown when trying to load " + case_dataset);
            		e1.printStackTrace();
                } catch (IOException e2) {
                    e2.printStackTrace();
                }

	            System.out.println("Finished processing cases:" + new Date(System.currentTimeMillis()));
            }
            if(calculateProbabilityOfEachVariableCompleteInfo)
            {
            	try {
            		BufferedReader br = new BufferedReader(new FileReader(full_dataset));
                    FileWriter writer = new FileWriter(data_folder + "\\Output_validation_complete_" + training_site + "_" + testing_site + "_" + new SimpleDateFormat("yyyyMMddHHmmss").format(new Date(System.currentTimeMillis())) + ".csv", true);
		            System.out.println("Finished loading cases: " + new Date(System.currentTimeMillis()));
		            String header = br.readLine();
		            header = header.replaceAll("\"","");
		            List<String> variableNames = Arrays.asList(header.split(","));
		            int numberOfNodes = 0;
		            long numberOfCases = 0;
		            NodeList nodes = domain.getNodes();
		            Map<String, Domain> domainsPerNode = new HashMap<String, Domain>();

		            // Headers
		            writer.write ("Researchnumber,");
		            //writer.write ("Researchnumber,PBD,");
		            for(Object nodeObject : nodes)
		            {
			            Node node = (Node)nodeObject;
	            		String nodeName = node.getName();
	            		if(!nodeName.equals("Anatomic_Tumor_Location") &&
	            				!nodeName.equals("T_Stage") && 
	            				!nodeName.equals("N_Stage") && 
	            				!nodeName.equals("M_Stage") && 
	            				!nodeName.equals("Treatment_Intent") &&
	            				variableNames.indexOf(nodeName) > -1)
	            		{
	            			writer.write (nodeName + ",");
	            			numberOfNodes++;
	            			String nodeCaseDataset =  tmp_folder + "Hugin-BN-RT-QA-" + testing_site + "_test_"+nodeName+".dat";
	            			Domain nodeDomain = new Domain (netName + ".net", parseListener);
	            			nodeDomain.triangulate (Domain.H_TM_BEST_GREEDY);
	            			nodeDomain.compile();
	    		            nodeDomain.parseCases (nodeCaseDataset, parseListener);
	    		            numberOfCases = nodeDomain.getNumberOfCases();
	            			domainsPerNode.put(nodeName, nodeDomain);
	            		}
		            }
		            System.out.println("Number of nodes: " + numberOfNodes);
		            writer.write ("incompatible_evidence,errors");
            		writer.write ("\n");
		            
		            for(int i = 0; i < numberOfCases; ++i)
		            {
		            	String line = br.readLine();
		                String[] lineItems = line.split(",");
		            	try {
			            	// Researchnumber
			            	writer.write (lineItems[0] + ",");
			            	
			            	// PBD
			            	//writer.write (lineItems[1] + ",");

		            		//domain.enterCase(i);
			            	//domain.propagate (Domain.H_EQUILIBRIUM_SUM, Domain.H_EVIDENCE_MODE_NORMAL);
			            	
			            	for(Object nodeObject : nodes)
				            {
			            		String nodeName = ((Node)nodeObject).getName();
			            		if(!nodeName.equals("Anatomic_Tumor_Location") &&
			            				!nodeName.equals("T_Stage") && 
			            				!nodeName.equals("N_Stage") && 
			            				!nodeName.equals("M_Stage") && 
			            				!nodeName.equals("Treatment_Intent"))
			            		{
			            			String nodeValue = "";
			            			if(variableNames.indexOf(nodeName) > -1)
			            			{
				            			Domain nodeDomain = domainsPerNode.get(nodeName);
				            			nodeDomain.enterCase(i);
				            			nodeDomain.propagate (Domain.H_EQUILIBRIUM_SUM, Domain.H_EVIDENCE_MODE_NORMAL);
					            		Node node = nodeDomain.getNodeByName(nodeName);

			            				nodeValue = lineItems[variableNames.indexOf(nodeName)].replace("\"","");
				            				
				            			NodeList justTheNode = new NodeList();
				    	            	justTheNode.add(node);
				    	            	Table marginal = nodeDomain.getMarginal(justTheNode);
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
				    	            		if(!"NULL".equals(nodeValue))
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
			            	writer.write(lineItems[variableNames.indexOf("errors")]);
	    	            	writer.write ("\n");
			            	
		            	} catch (ExceptionHugin e) {
			                System.out.println ("Exception at case " + i + ": " + e.getMessage());
			                for(int j=0; j<numberOfNodes; ++j)
			                	writer.write ("NaN,");
			                writer.write("1,");
			                writer.write(lineItems[variableNames.indexOf("errors")]);
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
