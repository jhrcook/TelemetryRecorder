//
//  PostWorkoutView.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import SwiftUI




struct PostWorkoutView: View {
    
    var dataManager: TelemetryDataManager
    
    @Environment(\.presentationMode) var presentationMode
    @State var saveFileStatus: TaskStatus = .incomplete
    @State var transferFileStatus: TaskStatus = .incomplete
    
    @State private var checkCancel = false
    
    var body: some View {
        VStack {
            
            Spacer()
            
            TaskCheckView(text: "Saved file", status: $saveFileStatus)
            TaskCheckView(text: "Transfered file", status: $transferFileStatus)
            
            Spacer()
            
            Button(action: {
                saveAndSyncData()
                presentationMode.wrappedValue.dismiss()
            }, label: {
                Text("Save data")
            })
            Button(action: {
                presentationMode.wrappedValue.dismiss()
            }, label: {
                Text("Cancel")
            })
        }
    }
}


struct PostWorkoutView_Previews: PreviewProvider {
    static var previews: some View {
        PostWorkoutView(dataManager: TelemetryDataManager())
    }
}
