package kdd;

import java.util.ArrayList;

import org.opencv.core.Mat;

/**
 * Description about feature: A Complete Optical Character Recognition Methodology for Historical Documents 
 *
 */

public class ZoneDensityFeature {
	private int numberOfRows = 5;
	private int numberOfColumns = 5;
	
	ZoneDensityFeature(int rows, int columns) {
		numberOfRows = rows;
		numberOfColumns = columns;
	}
	
	ArrayList<Integer> computeFeature(Mat image) {
		ArrayList<Integer> featureVector = new ArrayList<Integer>();
		int cellWidth = image.cols() / numberOfColumns;
		int cellHeight = image.rows() / numberOfRows;
		
		cellWidth = Math.max(cellWidth, 1);
		cellHeight = Math.max(cellHeight, 1);
		
		int[][] featureMatrix = new int[numberOfRows][numberOfColumns];
		for(int i = 0 ; i < numberOfRows; i++) {
			for(int j = 0 ; j < numberOfColumns; j++) {
				featureMatrix[i][j] = 0;
			}
		}
		
		for(int i = 0 ; i < image.rows(); i++) {
			for(int j = 0 ; j < image.cols(); j++) {
				//Get current pixel
				byte[] pixel = new byte[1];
				image.get(i, j, pixel);
				
				//Determine pixel (i,j) belongs to which featureMatrix cell
				int row = i / cellHeight;
				if (row >= numberOfRows) {
					row = numberOfRows - 1;
				}
				
				int col = j / cellWidth;
				if (col >= numberOfColumns) {
					col = numberOfColumns - 1;
				}
				
				if (pixel[0] != 0) {
					featureMatrix[row][col]++;
				}
			}
		}
		
		for(int i = 0 ; i < numberOfRows; i++) {
			for(int j = 0 ; j <numberOfColumns; j++) {
				featureVector.add(featureMatrix[i][j]);
			}
		}
		
		return featureVector;
	}
}
