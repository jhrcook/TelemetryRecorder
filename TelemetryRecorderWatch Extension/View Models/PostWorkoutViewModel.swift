//
//  PostWorkoutViewModel.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation

extension PostWorkoutView {
    
    func saveAndSyncData() {
        
        saveFileStatus = .incomplete
        transferFileStatus = .incomplete
        
        DispatchQueue.global(qos: .userInitiated).async {
            DispatchQueue.main.async { saveFileStatus = .inProgress }
            saveDataToFile()
            DispatchQueue.main.async { saveFileStatus = .complete }
            
            DispatchQueue.main.async { transferFileStatus = .inProgress }
            transferDataToWatch()
            DispatchQueue.main.async { transferFileStatus = .complete }
        }
    }
    
    
    func saveDataToFile() {
        print("Saving data to file...")
        dataManager.saveDataToFile()
        print("saved to: \(dataManager.saveFileURL)")
    }
    
    func transferDataToWatch() {
        print("Transfering data to phone...")
        
        // TODO
        
        print("Done.")
    }
}
