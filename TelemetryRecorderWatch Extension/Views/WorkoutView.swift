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
            
            HStack {
                Spacer()
                HStack {
                    Image(systemName: "hand.raised")
                    Text(": \(amountOfDataCollected)")
                        .foregroundColor(.green)
                        .bold()
                }
                Spacer()
                HStack {
                    Image(systemName: "waveform.path.ecg")
                    Text(": \(workoutManager.numberOfWorkoutDataPoints)")
                        .foregroundColor(.green)
                        .bold()
                }
                Spacer()
            }
            .padding(.bottom, 5)
            
            HStack {
                Spacer()
                Image(systemName: "heart.circle")
                    .foregroundColor(.red)
                Text("HR: ")
                Text("\(Int(workoutManager.heartrate))")
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
            PostWorkoutView(dataSaver: DataSaver(workoutInfo: workoutManager.info,
                                                 telemetryData: dataManager,
                                                 workoutData: workoutManager),
                            watchCommunicator: watchCommunicator)
        }
    }
}



struct WorkoutView_Previews: PreviewProvider {
    static let workoutChoices = WorkoutChoices()
    static var previews: some View {
        WorkoutView(workoutManager: WorkoutManager(info: workoutChoices.workouts[0]), watchCommunicator: WatchConnectivityManager())
    }
}
