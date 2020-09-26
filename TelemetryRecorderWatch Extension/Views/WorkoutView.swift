//
//  WorkoutView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI
import CoreMotion


struct WorkoutView: View {
    
    @ObservedObject var workoutManager: WorkoutManager
    var dataManager = TelemetryDataManager()
    
    let motionManager = CMMotionManager()
    let queue = OperationQueue()
    
    @State var amountOfDataCollected: Int = 0
    
    @State private var workoutComplete = false
    
    var body: some View {
        ZStack {
            VStack {
                Spacer()
                
                Text("Number of telemetry data points")
                    .multilineTextAlignment(.center)
                    .padding(.vertical, 5)
                Text("\(amountOfDataCollected)")
                    .font(.title)
                    .foregroundColor(.green)
                
                Spacer()
                
                Button(action: {
                    stopMotionManagerCollection()
                    workoutComplete.toggle()
                }) {
                    Text("Done")
                }
            }
            .navigationBarBackButtonHidden(true)
            .onAppear {
                startMotionManagerCollection()
            }
            
            NavigationLink(
                destination: PostWorkoutView(dataManager: dataManager),
                isActive: $workoutComplete) {
                EmptyView()
            }
            .opacity(0)
        }
        .onAppear {
            dataManager.workoutInfo = workoutManager.info
        }
    }
}



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



struct WorkoutView_Previews: PreviewProvider {
    static let workoutChoices = WorkoutChoices()
    static var previews: some View {
        WorkoutView(workoutManager: WorkoutManager(info: workoutChoices.workouts[0]))
    }
}
