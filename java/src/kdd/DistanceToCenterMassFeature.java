package kdd;

import java.util.ArrayList;

import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.imgproc.Imgproc;
import org.opencv.imgproc.Moments;

public class DistanceToCenterMassFeature {
	private int numberOfRows = 5;
	private int numberOfColumns = 5;
	
	DistanceToCenterMassFeature(int rows, int columns) {
		numberOfRows = rows;
		numberOfColumns = columns;
	}
	
	ArrayList<Integer> computeFeature(Mat image) {
		ArrayList<Integer> featureVector = new ArrayList<Integer>();
		
		int cellWidth = image.cols() / numberOfColumns;
		int cellHeight = image.rows() / numberOfRows;
		
		cellWidth = Math.max(cellWidth, 1);
		cellHeight = Math.max(cellHeight, 1);
		
		int[] upperFeature = new int[numberOfColumns];
		int[] lowerFeature = new int[numberOfColumns];
		int[] leftFeature = new int[numberOfRows];
		int[] rightFeature = new int[numberOfRows];
		
		
		//Get the center mass of image
		Moments m = Imgproc.moments(image, false);
		Point p = new Point(m.get_m10()/m.get_m00() , m.get_m01()/m.get_m00());
		
		//Initialization
		for(int j = 0 ; j < numberOfColumns; j++) {
			upperFeature[j] = 0;
			lowerFeature[j] = 0;
		}
		
		for(int i = 0 ; i < numberOfRows; i++) {
			leftFeature[i] = 0;
			rightFeature[i] = 0;
		}
		
		for(int i = 0; i < image.rows(); i++) {
			for(int j = 0 ; j < image.cols(); j++) {
				
				byte[] pixel = new byte[1];
				image.get(i, j, pixel);
				
				if (pixel[0] != 0) {
					int row = i / cellHeight;
					if (row >= numberOfRows) {
						row = numberOfRows - 1;
					}
					
					int col = j / cellWidth;
					if (col >= numberOfColumns) {
						col = numberOfColumns - 1;
					}
					
					if (i < p.y) {
						//Belong to upper feature
						int dist = (int) Math.round(p.y - i);
						upperFeature[col] = Math.max(upperFeature[col], dist);
					} 
					else if (i >= p.y) {
						//Belong to lower feature
						int dist = (int) Math.round(i - p.y);
						lowerFeature[col] = Math.max(lowerFeature[col], dist);
					}
					
					if (j < p.x) {
						//Belong to left feature
						int dist = (int) Math.round(p.x - j);
						leftFeature[row] = Math.max(leftFeature[row], dist);
					}
					else if (j >= p.x) {
						//Belong to right feature
						int dist = (int) Math.round(j - p.x);
						rightFeature[row] = Math.max(rightFeature[row], dist);
					}
				}
			}
		}
		
		//Combine into one vector left --> upper --> right --> lower
		for(int i = 0; i < numberOfRows; i++) {
			featureVector.add(leftFeature[i]);
		}
		
		//upper
		for(int j = 0; j < numberOfColumns; j++) {
			featureVector.add(upperFeature[j]);
		}
		
		//Right
		for(int i = 0; i < numberOfRows; i++) {
			featureVector.add(rightFeature[i]);
		}
		
		//lower
		for(int j = 0; j < numberOfColumns; j++) {
			featureVector.add(lowerFeature[j]);
		}
		
		return featureVector;
	}
}
