//
//  WorkoutView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI
import CoreMotion


struct ImageAndCounter: View {
    
    let imageSystemName: String
    var counterValue: Int
    
    var body: some View {
        HStack {
            Image(systemName: imageSystemName)
            Text(": \(counterValue)")
                .foregroundColor(.green)
                .bold()
        }
    }
}


struct WorkoutView: View {
    
    @ObservedObject var workoutManager: WorkoutManager
    var watchCommunicator: WatchConnectivityManager
    var telemetryDataManager = TelemetryDataManager()
    
    let motionManager = CMMotionManager()
    let queue = OperationQueue()
    
    @State var amountOfDataCollected: Int = 0
    
    @State internal var workoutComplete = false
    @State internal var presentTransferSheet = false
    
    @Environment(\.presentationMode) var presentationMode
    
    var body: some View {
        VStack {
            Spacer()
            
            HStack {
                Spacer()
                ImageAndCounter(imageSystemName: "hand.raised",
                                counterValue: amountOfDataCollected)
                Spacer()
                ImageAndCounter(imageSystemName: "waveform.path.ecg",
                                counterValue: workoutManager.numberOfWorkoutDataPoints)
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
            
            Button(action: stopMotionManagerCollection) {
                Text("Done")
            }
        }
        .navigationBarBackButtonHidden(true)
        .onAppear(perform: viewIsAppearing)
        .sheet(isPresented: $presentTransferSheet, onDismiss: {
            presentationMode.wrappedValue.dismiss()
        }) {
            PostWorkoutView(dataSaver: DataSaver(workoutInfo: workoutManager.info,
                                                 telemetryData: telemetryDataManager,
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
