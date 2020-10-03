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
            
            telemetryDataManager.updateMotionData(data: data, at: Date())
            DispatchQueue.main.async {
                amountOfDataCollected = telemetryDataManager.numberOfTelemetryDataPoints
            }
        }
    }
    
    
    func stopMotionManagerCollection() {
        workoutManager.endWorkout()
        motionManager.stopDeviceMotionUpdates()
        workoutComplete = true
        presentTransferSheet = true
    }
    
    internal func viewIsAppearing() {
        telemetryDataManager.reset()
        if !workoutComplete {
            telemetryDataManager.workoutInfo = workoutManager.info
            workoutManager.startWorkout()
            startMotionManagerCollection()
        }
    }
}
