package DisplayCluster;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.sql.Connection;

public class DisplayCluster {

	private  Connection conn;
	
	public DisplayCluster()
	{
		connect();
	}
	
	public void connect()
	{
		String url = "jdbc:mysql://164.15.78.25:3306/";
		
		String dbName = "vesale";
		String driver = "com.mysql.jdbc.Driver";
		String userName = "mysqluser"; 
		String password = "userul8mys9l";
		try { 
			Class.forName(driver).newInstance(); 
			 conn = DriverManager.getConnection(url+dbName,userName,password); 
		}
		
		catch (Exception e) 
		{ e.printStackTrace(); 
		} 
		

	}
	
	public void closeConnection()
	{
		try {
			conn.close();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
	public ArrayList<String> getImagesCluster(int cluster_id, int cluster_nr)
	{
		ArrayList<String> urls = new ArrayList<String>();
		try {
			PreparedStatement p= conn.prepareStatement("SELECT clusterID as id_cluster_algorithm,page,cluster as cluster_nr,imageID,replace(url,\"./\",\"http://164.15.78.25/dm/\")as url FROM cluster_index join vesale.image_cluster using(clusterID) left join image_index using(imageID) where cluster_index.clusterid=? and cluster=?");
			p.setInt(1, cluster_id);
			p.setInt(2, cluster_nr);
			ResultSet res=p.executeQuery();
			
			while(res.next())
			{
				String url=res.getString(5);
				urls.add(url);
			}
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return urls;
	}
	public void generateHTML(int cluster_id)
	{
		int numClusters=getNumberOfCluster(cluster_id);
		String label=getClusterLabel(cluster_id);
		for (int i = 1; i <= numClusters; i++)
		{
			createHtmlCluster(cluster_id, i, label, getImagesCluster(cluster_id,i));		
		}
	}
	
	private String getClusterLabel(int cluster_id) {
		String clusterLabel="";	
		try
		{
			
			String query="select cluster_label,descriptor from cluster_index where clusterid=?";
			PreparedStatement p= conn.prepareStatement(query);
			p.setInt(1,cluster_id );
			ResultSet res=p.executeQuery();
			
			while(res.next())
			{
				clusterLabel=res.getString(1)+" - Image descriptor:"+res.getString(2);
			}
		}
		catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return clusterLabel;
	}

	private int getNumberOfCluster(int cluster_id) {
		
		int numCluster=0;
		
		try
		{
			
			String query="SELECT count(distinct(cluster)) FROM vesale.image_cluster where clusterid=?";
			PreparedStatement p= conn.prepareStatement(query);
			p.setInt(1,cluster_id );
			ResultSet res=p.executeQuery();
			
			while(res.next())
			{
				numCluster=res.getInt(1);
			}
		}
		catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return numCluster;
	}

	public void createHtmlCluster(int cluster_id, int cluster_nr, String cluster_label, ArrayList<String>imageURL)
	{
			
		try 
		{
			PrintWriter pw = new PrintWriter("./data/"+cluster_id+"_"+cluster_nr+".html");
			pw.println("<!DOCTYPE html>");
			pw.println("<html lang=\"en\">");
			pw.println("<head><title>"+cluster_label+"</title></head>");
			pw.println("<body><h1>"+cluster_label+" </h1>");
			pw.println("<h2>Images of cluster# "+cluster_nr+" </h2>");
			pw.println("<h2>Total # of images: "+imageURL.size()+" </h2>");
			
			for (String url : imageURL) {
				pw.println(" <img src=\""+url+"\" alt=\"Some image\">");
			}
			
			pw.println("</body>");
			pw.println("</html>");
		
			pw.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}
	public void updateImages()
	{
		try
		{
			ArrayList<String> descriptors=getDescriptors();
			for (String desc : descriptors) {
				String query="update vesale.descriptors set num_images=(SELECT count(*) FROM vesale.DESCTABLE) where descriptor_table=?";
				
				PreparedStatement p= conn.prepareStatement(query.replaceAll("DESCTABLE", desc));
				p.setString(1,desc );
				p.executeUpdate();
				
			}
		}
		catch(SQLException e)
		{
			e.printStackTrace();
		}
		
		
	}
	
	private ArrayList<String> getDescriptors() {
		ArrayList<String> descriptors = new ArrayList<String>();
		try {
			PreparedStatement p= conn.prepareStatement("SELECT descriptor_table FROM vesale.descriptors");
			
			ResultSet res=p.executeQuery();
			
			while(res.next())
			{
				String desc=res.getString(1);
				descriptors.add(desc);
			}
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return descriptors;
		}
	
	
	public void updateClusterStats()
	{
		try
		{
			ArrayList<Integer> cluster_algorithms=getClusterAlgorithms();
			for (Integer ca : cluster_algorithms) {
				String query="update cluster_index set num_images=(select count(*) from image_cluster where ClusterID=?) where ClusterID=?";
				
				PreparedStatement p= conn.prepareStatement(query);
				
				p.setInt(1,ca);
				p.setInt(2,ca);
				System.out.println(p.toString());
				p.executeUpdate();
				
			}
		}
		catch(SQLException e)
		{
			e.printStackTrace();
		}
		
		
	}
	
	private ArrayList<Integer> getClusterAlgorithms() {
		ArrayList<Integer> ca = new ArrayList<Integer>();
		try 
		{
			
			PreparedStatement p= conn.prepareStatement("SELECT clusterID FROM vesale.cluster_index");
			
			ResultSet res=p.executeQuery();
			
			while(res.next())
			{
				int clus=res.getInt(1);
				ca.add(clus);
			}
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return ca;
		}

	public static void main(String[] args) {

			DisplayCluster dc= new DisplayCluster();
			//dc.createHtmlCluster(1, 1, "Test", dc.getImagesCluster(1));
			//dc.generateHTML(1000004);
			dc.updateClusterStats();
	}

}
