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
                amountOfDataCollected = dataManager.numberOfHardwareDataPoints
            }
        }
    }
    
    
    func stopMotionManagerCollection() {
        motionManager.stopDeviceMotionUpdates()
    }
}


// MARK: - Mock data

extension WorkoutView {
    func addMockData(N: Int = 1000) {
        DispatchQueue.global().async {
            var data = [HardwareDataPoint]()
            for _ in 0..<N {
                let dp = HardwareDataPoint(
                    pitch: Double.random(in: -1...1),
                    yaw: Double.random(in: -1...1),
                    roll: Double.random(in: -1...1),
                    accelX: Double.random(in: -1...1),
                    accelY: Double.random(in: -1...1),
                    accelZ: Double.random(in: -1...1)
                )
                data.append(dp)
            }
        }
        amountOfDataCollected = N
    }
    
    
    func randomDoubles(_ n: Int, range: ClosedRange<Double> = -1.0...1.0) -> [Double] {
        return (0..<n).map {_ in Double.random(in: range) }
    }
}
