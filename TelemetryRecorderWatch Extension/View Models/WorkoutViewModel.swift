//
//  WorkoutViewModel.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/27/20.
//

import Foundation
import CoreMotion


extension WorkoutView {
    func startMotionManagerCollection() {
        
        motionManager.startDeviceMotionUpdates(to: queue) { (data: CMDeviceMotion?, error: Error?) in
            guard let data = data else {
                if let error = error {
                    print("Error: \(error.localizedDescription)")
                } else {
                    print("Error in data collection but no error thrown.")
                }
                return
            }
            
            dataManager.updateMotionData(data: data)
            DispatchQueue.main.async {
                amountOfDataCollected = dataManager.numberOfHardwareDatapoints
            }
        }
        
        motionManager.startAccelerometerUpdates(to: queue) { (data: CMAccelerometerData?, error: Error?) in
            guard let data = data else {
                if let error = error {
                    print("Error: \(error.localizedDescription)")
                } else {
                    print("Error in data collection but no error thrown.")
                }
                return
            }
            
            dataManager.updateAccelerationData(data: data)
        }
        
    }
    
    
    func stopMotionManagerCollection() {
        motionManager.stopDeviceMotionUpdates()
        motionManager.stopAccelerometerUpdates()
    }
}

