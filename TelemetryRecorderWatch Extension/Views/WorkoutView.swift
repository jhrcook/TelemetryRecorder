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
    var watchCommunicator: WatchConnectivityManager
    
    var dataManager = TelemetryDataManager()
    
    let motionManager = CMMotionManager()
    let queue = OperationQueue()
    
    @State var amountOfDataCollected: Int = 0
    
    @State private var workoutComplete = false
    @State private var presentTransferSheet = false
    
    @Environment(\.presentationMode) var presentationMode
    
    var body: some View {
        VStack {
            Spacer()
            
            Text("Number of telemetry data points")
                .multilineTextAlignment(.center)
                .padding(.bottom, 5)
            
            ZStack {
                if (workoutComplete) {
                    Text("--")
                        .font(.title)
                        .foregroundColor(.green)
                } else {
                    Text("\(amountOfDataCollected)")
                        .font(.title)
                        .foregroundColor(.green)
                }
            }
            .padding(.bottom, 5)
            
            HStack {
                Spacer()
                Image(systemName: "heart.circle")
                    .font(.headline)
                    .foregroundColor(.red)
                Text("HR: ")
                    .foregroundColor(.white)
                    .font(.headline)
                Text(String(format: "%.1f", workoutManager.heartrate))
                    .font(.headline)
                    .bold()
                    .foregroundColor(.red)
                Spacer()
            }
            
            Spacer()
            
            Button(action: {
                workoutManager.endWorkout()
                stopMotionManagerCollection()
                workoutComplete = true
                presentTransferSheet = true
            }) {
                Text("Done")
            }
        }
        .navigationBarBackButtonHidden(true)
        .onAppear {
            dataManager.reset()
            if !workoutComplete {
                dataManager.workoutInfo = workoutManager.info
                workoutManager.startWorkout()
                startMotionManagerCollection()
            }
        }
        .sheet(isPresented: $presentTransferSheet, onDismiss: {
            presentationMode.wrappedValue.dismiss()
        }) {
            PostWorkoutView(dataManager: dataManager, watchCommunicator: watchCommunicator)
        }
    }
}



struct WorkoutView_Previews: PreviewProvider {
    static let workoutChoices = WorkoutChoices()
    static var previews: some View {
        WorkoutView(workoutManager: WorkoutManager(info: workoutChoices.workouts[0]), watchCommunicator: WatchConnectivityManager())
    }
}
