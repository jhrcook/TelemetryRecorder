//
//  WorkoutSetupView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/28/20.
//

import SwiftUI

struct WorkoutSetupView: View {
    
    var watchCommunicator: WatchConnectivityManager
    var workoutManager: WorkoutManager
    
    @State private var showWorkoutList = false
    @State var workoutInfo: WorkoutInformation? = nil
    
    @State private var showDurationPicker = false
    
    var body: some View {
        
        VStack {
            
            Button(action: {
                showWorkoutList.toggle()
            }) {
                Text(workoutInfo?.name ?? "none")
            }
            .sheet(isPresented: $showWorkoutList, onDismiss: {
                workoutManager.info = workoutInfo
            }) {
                WorkoutListView(workoutSelected: $workoutInfo)
            }
            
            Button(action: {
                showDurationPicker.toggle()
            }, label: {
                ZStack {
                    if (workoutInfo == nil) {
                        Text("none")
                    } else {
                        Text("\(workoutInfo!.duration) \(workoutInfo!.type == .count ? "times" : "sec")")
                    }
                }
            })
            .disabled(workoutInfo == nil)
            .sheet(isPresented: $showDurationPicker, onDismiss: {
                workoutManager.info = workoutInfo
            }) {
                WorkoutDurationSelectionView(workoutInformation: $workoutInfo)
            }
            
            Spacer()
            
            NavigationLink(
                destination: WorkoutView(workoutManager: workoutManager,
                                         watchCommunicator: watchCommunicator)) {
                Text("Start").foregroundColor(.blue).bold()
            }
            .disabled(workoutInfo == nil)
        }
        .onAppear {
            workoutInfo = WorkoutChoices().workouts[0]
            workoutManager.info = workoutInfo
        }
        .onDisappear {
            print("workoutManager.info.name: \(workoutManager.info?.name ?? "nil")")
        }
    }
}

struct WorkoutSetupView_Previews: PreviewProvider {
    static var previews: some View {
        WorkoutSetupView(watchCommunicator: WatchConnectivityManager(), workoutManager: WorkoutManager(info: nil))
    }
}
