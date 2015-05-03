package kdd;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.highgui.Highgui;

public class DensityFeature {
	
	public void printImage(Mat image) {
		int sumX = 0;
		int sumY = 0;
		int count = 0;
		
		for(int i = 0 ; i <= image.rows() + 1; i++) {
			for(int j = 0 ; j <= image.cols() + 1; j++) {
				byte[] tmp = new byte[1];
				image.get(i, j, tmp);
				
				if (tmp[0] != 0) {
					tmp[0] = 1;
				}
				
				if (i == 19 || j == 8) {
					tmp[0] = 5;
				}
				
				if (tmp[0] != 0) {
					sumX += i;
					sumY += j;
					count++;
				}
				
				System.out.print(tmp[0] + " ");
			}
			System.out.println();
		}
		
		System.out.println(((double)sumX/count) + " " +  ((double)sumY/count));
	}

	public static void main(String[] args) throws IOException {
		
		int startPage = 4;
		int endPage = 25;
		String folderPath = "D:/IT4BI/Semester 2/Knowledge Discovery and Data Mining/Project/kdd/thumbs";
		
		Pattern imagePattern = Pattern.compile("img-(\\d+).png");
		
		if (args.length > 3) {
			folderPath = args[0];
			startPage = Integer.parseInt(args[1]);
			endPage = Integer.parseInt(args[2]);
		}
		
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME );
		ZoneDensityFeature zdf = new ZoneDensityFeature(5 , 5);
		DistanceToCenterMassFeature dtcmf = new DistanceToCenterMassFeature(5, 5);
		
		BufferedWriter fileWriter = new BufferedWriter(new FileWriter("densityFeature.csv"), 65536);
		
		for(int page = startPage; page <= endPage; page++) {
			long startTime = System.currentTimeMillis();
			
			File imageFolder = new File(folderPath + "/" + page);
			File[] listOfFiles = imageFolder.listFiles();
			
			int fileCount = 0;
			
			if (listOfFiles != null) {
				for(File file : listOfFiles) {
					
					//Check if the file is an image
					String fileName = file.getName();
					Matcher imageMatcher = imagePattern.matcher(fileName);
					
					if (imageMatcher.matches()) {
						
						fileCount++;
						
						//Found an image
						int imageId = Integer.parseInt(imageMatcher.group(1));
						String imagePath = folderPath + "/" + page + "/" + fileName;
						
						//Read image
						Mat image = Highgui.imread(imagePath, 0);
						
						//Process page
						ArrayList<Integer> zdfArray =  zdf.computeFeature(image);
						ArrayList<Integer> dtcmfArray = dtcmf.computeFeature(image);
						
						fileWriter.write(imageId + "\t");
						for(int i = 0; i < zdfArray.size(); i++) {
							if (i > 0) {
								fileWriter.write(',');
							}
							fileWriter.write(zdfArray.get(i) + "");
						}
						
						for(int i = 0 ; i < dtcmfArray.size(); i++) {
							fileWriter.write(',');
							fileWriter.write(dtcmfArray.get(i) + "");
						}
						fileWriter.write("\n");
					}
				}
			}
			
			long endTime   = System.currentTimeMillis();
			long totalTime = endTime - startTime;
			System.out.println("Processed page: " + page + " in " + totalTime + " miliseconds");
			System.out.println("Files: " + fileCount);
		}
		
		fileWriter.close();
	}

}
