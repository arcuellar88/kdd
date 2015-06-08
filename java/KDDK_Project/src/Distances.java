import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.aliasi.io.FileLineReader;

public class Distances {
	private int clusterIDs[];
	private String clusterLabels[];
	private double clusterMeans[][];
	private int imageIDs[];
	private String imageLabels[];
	private int imageCHs[][];
	private int imageDFs[][];
	private int imageData[][];
	private final int TOTAL_IMAGES=1843; //++++++
	private final int FIRST_ID=2; //+++++
	private final int DATA_LENGTH=75;
	private final String DISTANCE_TYPE = "e"; //e for euclidean, ma for manhattan, mi for minkowski
	
	public Distances(){
		this.imageIDs = new int[this.TOTAL_IMAGES+this.FIRST_ID+1]; //so every image data is on the same index
		this.imageCHs = new int[this.TOTAL_IMAGES+this.FIRST_ID+1][30];
		this.imageDFs = new int[this.TOTAL_IMAGES+this.FIRST_ID+1][45];
		this.imageData = new int[this.TOTAL_IMAGES+this.FIRST_ID+1][DATA_LENGTH];
		this.imageLabels = new String[this.TOTAL_IMAGES+this.FIRST_ID+1];
	}
	
	public void readDescriptors(){
		File feat_cv_30 = new File("feat-cv-y-profile_30.txt");
		File feat_df_30 = new File("densityFeature_Zone5x5_Distance5x5.csv");
		String[] cv = null;
		String[] df = null;
		try{
			cv = FileLineReader.readLineArray(feat_cv_30,"UTF-8");
			df = FileLineReader.readLineArray(feat_df_30,"UTF-8");

			for(String line : cv){
				String elems[] = line.split("\t");
				int imgID = Integer.parseInt(elems[0]);
				imageIDs[imgID] = imgID;
				String chs[] = elems[1].split(",");
				for(int j=0;j<chs.length;j++){
					this.imageCHs[imgID][j] = Integer.parseInt(chs[j]);
				}
			}
			for(String line : df){
				String elems[] = line.split("\t");
				int imgID = Integer.parseInt(elems[0]);
				//imageIDs[imgID] = imgID;
				String dfs[] = elems[1].split(",");
				for(int j=0;j<dfs.length;j++){
					this.imageDFs[imgID][j] = Integer.parseInt(dfs[j]);
				}
			}
			for(int j=0; j<this.imageIDs.length; j++){
				for(int k=0; k<this.imageDFs[j].length; k++){
					this.imageData[j][k] = this.imageDFs[j][k];
				}
				for(int k=0; k<this.imageCHs[j].length; k++){
					this.imageData[j][k+this.imageDFs[j].length] = this.imageCHs[j][k];
				}
			}
			
		}catch(Exception e){}
		System.out.println("descriptors read");
	}
	
	public void associateCluster(){
		System.out.println("associating clusters");
		String data = "";
		for(int i=0; i<this.imageIDs.length; i++){
			double minDist = getDistance(this.clusterMeans[1],this.imageData[i]); //dist to first cluster
			int clusterID=this.clusterIDs[1];
			for(int j=2; j<this.clusterMeans.length; j++){
				double newDist = getDistance(this.clusterMeans[j],this.imageData[i]); //dist to next cluster
				if(newDist < minDist){
					clusterID = this.clusterIDs[j]; //set as new associated cluster
					minDist = newDist;
				}
			}
			this.imageLabels[i] = this.clusterLabels[clusterID];
			//System.out.println(this.imageIDs[i] + "\t" + this.imageLabels[i]);
			data += this.imageLabels[i] + " ";
		}
		String newdata = data.replace("n  RightN", "m");
		newdata = newdata.replace("DownS  o", "g");
		newdata = newdata.replace("i  RightN", "n");
		System.out.print(newdata);
	}
	
	public double getDistance(double cl[], int img[]){
		
		double dist = 0.0;
		double sum = 0.0;
		if(this.DISTANCE_TYPE.equalsIgnoreCase("e")){ //if eculidean distance
			
			for(int i=0; i< cl.length; i++){
				double diffPow = Math.pow(((double)img[i] - cl[i]),2); //(a-b)^2
				sum += diffPow;
			}
			dist = Math.sqrt(sum);
		}else{
			if(this.DISTANCE_TYPE.equalsIgnoreCase("ma")){ //if manhattan distance
				for(int i=0; i< cl.length; i++){
					double diffAbs = Math.abs(((double)img[i] - cl[i])); //|a-b|
					sum += diffAbs;
				}
				dist = sum;
			}else{ //minkowski distance
				double p=0.5; //test
				for(int i=0; i< cl.length; i++){
					double diffAbs = Math.abs(((double)img[i] - cl[i])); //|a-b|
					double diffAbsPow = Math.pow(diffAbs, p);
					sum += diffAbsPow;
				}
				dist = Math.pow(sum, (1/p));
			}
			
		}
		return dist;
	}
	
	public double[] getNumbers(String a[]){
		double numbers[] = new double[a.length];
		for(int i=0; i<a.length;i++){
			numbers[i] = Double.parseDouble(a[i]);
		}
		return numbers;
	}
	
	
	public void loadMeans(){
		File clusters = new File("MEANS.txt");
		String[] lines = null;
		this.clusterIDs = new int[1000+1];
		this.clusterLabels = new String[1000+1];
		this.clusterMeans = new double[1000+1][DATA_LENGTH];
		try{
			lines = FileLineReader.readLineArray(clusters,"UTF-8");
			for(String line : lines){
				String elems[] = line.split("\t"); //clusterID, label, means
				int clID=Integer.parseInt(elems[0]);
				this.clusterIDs[clID] = clID;
				this.clusterLabels[clID] = elems[1];
				String dfschs[] = elems[2].split(",");
				for(int j=0;j<dfschs.length;j++){
					this.clusterMeans[clID][j] = Double.parseDouble(dfschs[j]);
				}
				
			}
		}catch(Exception e){}
		System.out.println("means loaded");
	}
	
	/**
	 * AVERAGE OF EVERY COLUMN.
	 * e.g. [0.5, 1, 2] [0.5, 2, 2] ==> [0.5, 1.5, 2]
	 */
	public void calculateMeans(){ 
		File clusters = new File("clusters.csv");
		String[] lines = null;
		this.clusterIDs = new int[1000];
		this.clusterLabels = new String[1000];
		this.clusterMeans = new double[1000][DATA_LENGTH];
		try{
			lines = FileLineReader.readLineArray(clusters,"UTF-8");
			int currentCluster = 0;
			String currentLabel = "";
			int currentData[] = new int[DATA_LENGTH];
			int countElems = 1;
			for(String line : lines){
				String elems[] = line.split("\t"); //clusterID, label, imageID, DF+CH
				int newCluster = Integer.parseInt(elems[0]);
				String newLabel = elems[1];
				String dfschs[] = elems[3].split(",");
				int newData[] = new int[DATA_LENGTH];
				for(int j=0;j<dfschs.length;j++){
					newData[j] = Integer.parseInt(dfschs[j]);
				}
				if(currentCluster==141){
					System.out.println();
				}
				if(currentCluster==0){
					currentCluster = newCluster;
					currentLabel = newLabel;
					currentData = newData;
					countElems = 1;
				}else{
					if(newCluster != currentCluster){
						//calculate average and save
						this.clusterIDs[currentCluster-1] = currentCluster;
						this.clusterLabels[currentCluster-1] = currentLabel;
						for(int j=0; j<currentData.length; j++){
							this.clusterMeans[currentCluster-1][j] = (double)currentData[j]/countElems;
						}
						//make new Cluster the current cluster
						currentCluster = newCluster;
						currentLabel = newLabel;
						currentData = newData;
						countElems = 1;
					}else{
						for(int j=0; j<newData.length; j++){
							currentData[j] += newData[j];
						}
						countElems++;
					}
				}
				/*int currentData[] = new int[dfschs.length];
				for(int j=0;j<dfschs.length;j++){
					currentData[j] = Integer.parseInt(dfschs[j]);
				}
				*/
			}
			
			PrintWriter writer = new PrintWriter("MEANS.txt", "UTF-8");
			for(int i=0; i<this.clusterIDs.length; i++){
				String data="";
				for(int j=0; j<74; j++){
					data = data + this.clusterMeans[i][j] + ",";
				}
				data = data + this.clusterMeans[i][74];
				writer.println(this.clusterIDs[i]+"\t"+ this.clusterLabels[i]+"\t"+ data);
			}
			writer.close();
			
			
		}catch(Exception e){}
	}
	
	/**
	 * Generate HTML file for image classifcation
	 * @param templateFile
	 * @param imagePathPrefix
	 * @param Imageextension
	 * @throws IOException
	 */
	
	public void generateHTML(String templateFile, String imagePathPrefix, String Imageextension) throws IOException {
		
		StringBuilder htmlbody = new StringBuilder();
		
		//Build HTML from classification result
		for(int i=0; i<this.imageIDs.length; i++) {
			String imagePath = imagePathPrefix + this.imageIDs[i] + "." + Imageextension;
			htmlbody.append("<div class='image-container'>");
				htmlbody.append("<div class='image'>");
					htmlbody.append("<img src='" + imagePath + "'/>");
				htmlbody.append("</div>");
					
				htmlbody.append("<div class='label'>");
					htmlbody.append("<span>" + this.imageLabels[i] +"</span>");
				htmlbody.append("</div>");
			htmlbody.append("</div>");
		}
		
		byte[] encoded = Files.readAllBytes(Paths.get(templateFile));
		String htmlContent = new String(encoded, StandardCharsets.UTF_8);
		htmlContent = htmlContent.replaceAll("KDD_CLUSIFICATION_RESULT", htmlbody.toString());
		
		BufferedWriter fileWriter = new BufferedWriter(new FileWriter("result.html"), 65536);
		fileWriter.write(htmlContent);
		fileWriter.close();
	}
	
	public static void main(String a[]) throws IOException{
		Distances d = new Distances();
		//d.calculateMeans();
		d.loadMeans();
		d.readDescriptors();
		d.associateCluster();
		d.generateHTML("template.html", "text/img-", "png");
		System.out.println("FIN");
	}
	
}
