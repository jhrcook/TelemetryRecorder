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
            deleteDataFileFromWatch()
            DispatchQueue.main.async { transferFileStatus = .complete }
        }
    }
    
    
    func saveDataToFile() {
        print("Saving data to file: \(dataSaver.saveFileURL.lastPathComponent)")
        dataSaver.saveDataToFile()
        print("   done")
    }
    
    func transferDataToWatch() {
        print("Transfering data to phone...")
        watchCommunicator.transferToPhone(url: dataSaver.saveFileURL)
        while watchCommunicator.numberOfOutstandingFileTransfers > 0 {
            // Wait until transfers are complete.
        }
        print("   done")
    }
    
    func deleteDataFileFromWatch() {
        do {
            try FileManager.default.removeItem(at: dataSaver.saveFileURL)
        } catch {
            print("unable to delete data file: \(error.localizedDescription)")
        }
    }
}
