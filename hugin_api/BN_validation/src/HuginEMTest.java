import COM.hugin.HAPI.DefaultClassParseListener;
import COM.hugin.HAPI.DiscreteChanceNode;
import COM.hugin.HAPI.Domain;
import COM.hugin.HAPI.NodeList;
import COM.hugin.HAPI.ParseListener;

public class HuginEMTest {
	
	String homedir = "C:\\Users\\i.bermejo\\Documents";
	String net_folder = homedir + "\\Src\\bn-for-rt-plan-qa\\";

	/*
	 * This simple Java program illustrates how to learn (the parameters)
	 * of a Bayesian network from data
	 *
	 * The result is stored in a file name learning.net
	 *
	 *
	 * Author: Anders L Madsen @ HUGIN EXPERT A/S
	 *
	 * For any questions or comments, please contact the author at
	 * alm@hugin.com
	 */
	static public void main (String args[])
	{
		HuginEMTest ede = new HuginEMTest ();
		ede.doLearning ();
	}

	// the model
	Domain dom = null;

	// the nodes / variables in the model
//	LabelledDCNode Sex, Height;

	public HuginEMTest ()
	{
		try {
			// Create domain. Done once.
			ParseListener parseListener = new DefaultClassParseListener();
			dom = new Domain(net_folder + "asia.net", parseListener);
			
			NodeList nl = dom.getNodes();
			for(Object nodeObject : nl)
			{
				((DiscreteChanceNode)nodeObject).getExperienceTable();
			}
//			Sex = new LabelledDCNode(dom); Sex.setName ("Sex");
//			Height = new LabelledDCNode(dom); Height.setName ("Height");
//
//			Sex.setNumberOfStates (2);
//			Sex.setStateLabel (0, "Female");
//			Sex.setStateLabel (1, "Male");
//
//			Height.setNumberOfStates (2);
//			Height.setStateLabel (0, "Low");
//			Height.setStateLabel (1, "High");
//
//			// Add Sex as parent of Height. Comment this line if you
//			// uncomment "dom.learnStructure ();" below
//			Height.addParent (Sex);
//
//			// create Experience tables to enable CPT estimation
//			Sex.getExperienceTable ();
//			Height.getExperienceTable ();

		} catch (Exception e) {
			e.printStackTrace ();
			System.err.println (e.getMessage ());	   
		}
	}

	// Data is an array of strings. First entry is Sex and second is
	// Height;
//	String dataArray[][]  =
//		{
//				{"Male", "High"},
//				{"Male", "High"},
//				{"Male", "High"},
//				{"Female", "High"},
//				{"Female", "Low"},
//				{"Male", "Low"},
//				{"Female", "Low"},
//				{"Male", "High"},
//				{"Male", ""},
//				{"Female", "Low"}
//
//		};
//	;
//	int N=10;

	// Main procedure. 
	protected void doLearning () {
		try{
			// enter data
			//enterData ();
			ParseListener parseListener = new DefaultClassParseListener();
			dom.parseCases(net_folder + "asia.dat", parseListener);
			// (learn structure) compile and estimate parameters
			// dom.learnStructure (); // uncomment this line (and the
			// addParent line) above to learn structure from data
			dom.compile ();
			dom.learnTables ();

			// save domain in file
			dom.saveAsNet (net_folder + "asia_2.net");
		} catch (Exception e) {
			e.printStackTrace ();
			System.err.println (e.getMessage ());	   
		}
	}
//
//	//
//	protected void enterData () {
//		try{
//			long j, s;
//			for (int i = 0; i < N; i++) {
//				j = dom.newCase ();
//
//				s = Sex.getStateIndex (dataArray[i][0]);
//				if (s >= 0)
//					Sex.setCaseState (j, s);
//
//				s = Height.getStateIndex (dataArray[i][1]);
//				if (s >= 0)
//					Height.setCaseState (j, s);
//			}
//			//... more data
//		} catch (Exception e) {
//			e.printStackTrace ();
//			System.err.println (e.getMessage ());	   
//		}
//	}

}
