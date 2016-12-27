import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.* ;

public class CSVLoader {
				//Variables	
				private  static final String dbUrl = "jdbc:oracle:thin:@SERVER:PORT/SERVICE";
				private  static final String dbUsername = "USER";
				private  static final String dbPassword = "PASSWORD";								
				public static final String csvFile = "Test.csv";
				
    			
				private static Connection getConnection() throws SQLException
    		{
        	return DriverManager.getConnection(dbUrl, dbUsername, dbPassword);
    		}
				
				public Integer getMaxScoreModel() 
    		{
    			Integer maxId=new Integer(0);
    			try{
		        	Connection connection = null;
		        	connection= getConnection();
		        	Statement stmt=connection.createStatement();  
							ResultSet rs=stmt.executeQuery("select MAX(Fieid) from Table");  
							
							while(rs.next())  
									maxId= rs.getInt(1);  
					    connection.close(); 
					} catch (SQLException e) {

							System.out.println("Connection Failed! Check getMaxScoreModel()");
							e.printStackTrace();
							//return 0;
					}    
					return maxId;
    		}
				
				public String generateQuery(Integer Field)
        {
	        String line = "";
	        String cvsSplitBy = ",";
					String insertTableSQL="";
					
					//Integer i=1;
					
        	try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

            while ((line = br.readLine()) != null) {
							//i++;
                // use comma as separator
                String[] csvRead = line.split(cvsSplitBy);
                
								if (csvRead[0].equals(new String("\"Value\""))) continue;
								//if (i==5) break;
								insertTableSQL=insertTableSQL + " INTO TABLE (Field List)" ;
								insertTableSQL=insertTableSQL+ " VALUES (" + Field + ",'" + csvRead[0].replace("\"","") + "','" + csvRead[1].replace("\"","") + " ','" + csvRead[5].replace("\"","") + "'," + (csvRead[6]+000) + "," + csvRead[7] + "," + csvRead[8] + "," + csvRead[9] + "," +csvRead[10] + "," + csvRead[11] + "," + csvRead[12] + "," + csvRead[13] + "," + csvRead[14] + "," + csvRead[15] + "," + csvRead[16] + "," + csvRead[17] + "," + csvRead[18] + "," + (csvRead[19] + 000) + "," + csvRead[20] + "," + csvRead[21] + "," + csvRead[22] + "," + csvRead[23] + "," + csvRead[24] + "," + csvRead[25] + "," + csvRead[26] + "," + csvRead[27] + "," + (csvRead[28] + 000) + "," + (csvRead[29] + 000) + "," + (csvRead[30] + 000) + "," + (csvRead[31] + 000)+", 0)\n" ;;
							 
            }

        	} catch (IOException e) 
        	{
            System.out.println("CSV not found! Check Scored.csv in path");
						e.printStackTrace();
						//return insertTableSQL;
        	}
        	
        	return "INSERT ALL "+insertTableSQL+" SELECT * FROM dual";
    		}
    
    		public Integer insertIntoDB(String insertTableSQL) 
    		{
    			Integer nRow=new Integer(0);
    			try{
	        	Connection connection = null;
	        	connection= getConnection();
	        	Statement stmt=connection.createStatement();  
						nRow=stmt.executeUpdate(insertTableSQL);  
				  	connection.close(); 
					
					} catch (SQLException e) 
	        	{
	            System.out.println("Connection Failed! Check insertIntoDB()");
							e.printStackTrace();
							return 0;
	        }	
	        return nRow;
				}
    
    public static void main(String[] args) {	
    	Integer RUN_ID=0;	
    	Integer nRow=0;
    	String insertTableSQL="";
    	CSVLoader c = new CSVLoader();
    	RUN_ID=c.getMaxScoreModel();
    	//System.out.println(Run_ID);
    	insertTableSQL=c.generateQuery(++RUN_ID);
    	//System.out.println(insertTableSQL);
    	nRow=c.insertIntoDB(insertTableSQL);
    	System.out.println("Total Number of rows imported= "+nRow);
	 	}	
}
