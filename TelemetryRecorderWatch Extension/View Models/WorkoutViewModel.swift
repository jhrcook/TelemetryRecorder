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


// MARK: - Mock data

extension WorkoutView {
    func addMockData(N: Int = 1000) {
        DispatchQueue.global().async {
            dataManager.hardwareData.accelX = randomDoubles(N)
            dataManager.hardwareData.accelY = randomDoubles(N)
            dataManager.hardwareData.accelZ = randomDoubles(N)
            dataManager.hardwareData.pitch = randomDoubles(N)
            dataManager.hardwareData.roll = randomDoubles(N)
            dataManager.hardwareData.yaw = randomDoubles(N)
        }
        amountOfDataCollected = N
    }
    
    
    func randomDoubles(_ n: Int, range: ClosedRange<Double> = -1.0...1.0) -> [Double] {
        return (0..<n).map {_ in Double.random(in: range) }
    }
}
