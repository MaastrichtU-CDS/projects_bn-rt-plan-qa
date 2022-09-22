import COM.hugin.HAPI.DefaultClassParseListener;
import COM.hugin.HAPI.DiscreteChanceNode;
import COM.hugin.HAPI.Domain;
import COM.hugin.HAPI.ExceptionHugin;
import COM.hugin.HAPI.NodeList;
import COM.hugin.HAPI.ParseListener;
import COM.hugin.HAPI.Table;

public class EMAlgorithmHugin {

	public static void main(String[] args) {
		// Paths
		String homedir = "C:\\Users\\i.bermejo\\Documents";
		String data_folder = homedir + "\\Data\\BN in RT\\";
		String tmp_folder = data_folder + "tmp\\";
		String net_folder = homedir + "\\Src\\bn-for-rt-plan-qa\\";
		
		String site = "Maastro";
		// Net name
		String netName = net_folder + "RT_Plan_Error_detection_20220908_" + site;
		//String netName = net_folder + "asia";

		// Training dataset
		String training_set = data_folder + "Hugin-BN-RT-QA-"+ site + "_train.dat";
		//String training_set =  net_folder + "asia.dat";

		try {
			ParseListener parseListener = new DefaultClassParseListener();
			Domain d = new Domain(netName + ".net", parseListener);

			d.openLogFile(netName + ".log");
			d.triangulate (Domain.H_TM_BEST_GREEDY);
			specifyLearningParameters (d);
			printLearningParameters (d);

			d.parseCases (training_set, parseListener);

			d.compile ();
			d.learnTables ();

			System.out.println("Log likelihood: " + d.getLogLikelihood() );

			//printNodeMarginals (d);

			d.saveAsNet (netName + "_trained.net");
		}catch(Exception e)
		{
			e.printStackTrace();
		}

	}

	private static void specifyLearningParameters (Domain d) throws ExceptionHugin
	{
	  NodeList nl = d.getNodes ();

	  for (Object nodeObject : nl)
	  {
	    DiscreteChanceNode node = (DiscreteChanceNode)nodeObject;

	    if (node != null) {
	      Table table = node.getExperienceTable ();
	      double[] data =  table.getData();
	      for(int i=0; i<data.length; i++)
	      {
	    	  data[i] = 1.0/data.length;
	      }
	      table.setData (data);
	      
	      table = node.getTable();
	      data =  table.getData();
	      for(int i=0; i<data.length; i++)
	      {
	    	  data[i] = 1;
	      }
	      table.setData (data);
	    }
	  }

	  d.setLogLikelihoodTolerance (0.000001);
	  d.setMaxNumberOfEMIterations (1000);
	}


	private static void printLearningParameters (Domain d) throws ExceptionHugin
	{
	  NodeList nl = d.getNodes();

	  for (Object nodeObject : nl) {
	    DiscreteChanceNode dcNode = (DiscreteChanceNode)nodeObject;

	    if (dcNode != null) {
	      System.out.println(dcNode.getLabel () + " (" + dcNode.getName () + "):");

	      System.out.print("   ");
	      if (dcNode.hasExperienceTable ()) {
	    	  Table table = dcNode.getExperienceTable ();
	    	  double[] data = table.getData ();
	    	  long tblSize = table.getSize ();

	    	  for (int i = 0; i < tblSize; i++)
	    		  System.out.print(data[i] + " ");
    		  System.out.println();
	      }
	      else{
	    	  System.out.println("No experience table.");
	      }

	      System.out.print("   ");
	      if (dcNode.hasFadingTable ()) {
	    	  Table table = dcNode.getFadingTable ();
	    	  double[] data = table.getData ();
	    	  long tblSize = table.getSize ();

	    	  for (int i = 0; i < tblSize; i++)
	    		  System.out.print( data[i] + " ");

	    	  System.out.println();
	      }
	      else
	    	  System.out.println("No fading table");
	    }
	  }

	  System.out.println("Log likelihood tolerance: " + d.getLogLikelihoodTolerance ());
	  System.out.println("Max EM iterations: " + d.getMaxNumberOfEMIterations ());
	}


	static void loadCases (Domain d) throws ExceptionHugin
	{
	  d.setNumberOfCases (0);

	  long iCase = d.newCase ();
	  System.out.println("Case index: " + iCase);

	  d.setCaseCount (iCase, 2.5);

	  NodeList nl = d.getNodes ();

	  for (Object nodeObject : nl) {
	    DiscreteChanceNode dcNode = (DiscreteChanceNode)nodeObject;

	    if (dcNode != null)
	      dcNode.setCaseState (iCase, 0);
	  }

	  DiscreteChanceNode dcNode = (DiscreteChanceNode) nl.get(1);
	  if (dcNode != null)
	    dcNode.unsetCase (iCase);
	}


	static void printCases (Domain d) throws ExceptionHugin
	{
	  NodeList nl = d.getNodes ();

	  long nCases = d.getNumberOfCases ();

	  System.out.println("Number of cases: ");

	  for (int i = 0; i < nCases; i++) {
		  System.out.print("case " + i + " " + d.getCaseCount (i) + " ");

	    for (Object nodeObject : nl) {
	      DiscreteChanceNode dcNode = (DiscreteChanceNode)nodeObject;

	      if (dcNode != null) {
	    	  System.out.print(" (" + dcNode.getName () + ",");
	    	  if (dcNode.caseIsSet (i))
	    		  System.out.print(dcNode.getCaseState (i) + ") ");
	    		  else
	    			  System.out.print("N/A) ");
	      }
	    }
	  }
	  System.out.println();
	}


	private static void printNodeMarginals (Domain d) throws ExceptionHugin
	{
	  NodeList nl = d.getNodes ();

	  for (Object nodeObject : nl) {
	    DiscreteChanceNode dcNode = (DiscreteChanceNode)nodeObject;

	    if (dcNode != null) {
	      long nStates = dcNode.getNumberOfStates ();

	      System.out.println(dcNode.getLabel () + " (" + dcNode.getName () + ")");

	      for (int i = 0; i < nStates; i++)
	    	  System.out.println(" - " + dcNode.getStateLabel (i) + ": " + dcNode.getBelief (i));
	    }
	  }
	}

}
