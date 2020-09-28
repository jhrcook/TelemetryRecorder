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
    @State var dataTransferComplete = false
    
    @Environment(\.presentationMode) var presentationMode
    
    var body: some View {
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
            dataManager.workoutInfo = workoutManager.info
        }
        .onDisappear {
            stopMotionManagerCollection()
        }
        .sheet(isPresented: $workoutComplete, onDismiss: {
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
