//
//  ContentView.swift
//  TelemetryRecorder
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI

struct ContentView: View {
    
    @State private var watchConnectionStatus = "Watch not connected"
    @State private var statusMessage = "Idle"
    
    let icloudFileManager = iCloudFileManager()
    
    var body: some View {
        VStack {
            Button(action: {
                icloudFileManager.testMakingFile()
            }) {
                HStack {
                    Image(systemName: "arrow.up.doc")
                    Text("Upload file to iCloud")
                }
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
