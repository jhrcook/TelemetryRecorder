//
//  ContentView.swift
//  TelemetryRecorder
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI
import WatchConnectivity

struct ContentView: View {
    
    @State private var numberOfFilesReceived = 0
    
    let icloudFileManager = iCloudFileManager()
    let watchManager = PhoneToWatchConnectivityManager()
    
    @State private var sessionActivationState: WCSessionActivationState = .inactive
    @State private var sessionIsPaired = false
    @State private var sessionIsReachable = false
    
    var body: some View {
        VStack {
            
            Spacer()
            
            Text("Session activation state: \(sessionActivationState.rawValue)")
            Text("Session is paired: \(sessionIsPaired ? "True" : "False")")
            Text("Session is reachable: \(sessionIsReachable ? "True" : "False")")
            
            Spacer()
            
            Text("Number of files received: \(numberOfFilesReceived)")
            
            Spacer()
        }
    }
}


extension ContentView: PhoneToWatchConnectivityDelegate {
    func didRecieveNewDataFile(url: URL) {
        numberOfFilesReceived += 1
        icloudFileManager.saveTransferedData(url: url)
    }
    
    func watchStateDidChange(session: WCSession) {
        sessionIsPaired = session.isPaired
        sessionIsReachable = session.isReachable
        sessionActivationState = session.activationState
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
