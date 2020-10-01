//
//  ContentView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct ContentView: View {
    
    var watchCommunicator = WatchConnectivityManager()
    var workoutManager = WorkoutManager(info: nil)
    
    var body: some View {
        WorkoutSetupView(watchCommunicator: watchCommunicator, workoutManager: workoutManager)
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
