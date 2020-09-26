//
//  ContentView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct ContentView: View {
    
    var watchCommunicator = WatchConnectivityManager()
    
    @State private var showWorkoutList = false
    @State var workoutInfo: WorkoutInformation? = nil
    
    @State private var showDurationPicker = false
    
    var body: some View {
        
        VStack {
            HStack {
                Text("Workout").font(.footnote)
                Spacer()
            }
            Button(action: {
                showWorkoutList.toggle()
            }) {
                Text(workoutInfo?.name ?? "none")
            }
            .sheet(isPresented: $showWorkoutList, content: { WorkoutListView(workoutSelected: $workoutInfo) })
            
            Spacer()
            
            HStack {
                Text("Duration").font(.footnote)
                Spacer()
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
            .sheet(isPresented: $showDurationPicker) {
                WorkoutDurationSelectionView(workoutInformation: $workoutInfo)
            }
            
            Spacer()
            
            NavigationLink(
                destination: Text("Destination")) {
                Text("Start").foregroundColor(.blue).bold()
            }
            .disabled(workoutInfo == nil)
        }
        
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
