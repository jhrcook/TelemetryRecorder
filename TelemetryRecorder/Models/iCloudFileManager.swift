//
//  iCloudFileManager.swift
//  TelemetryRecorder
//
//  Created by Joshua on 9/26/20.
//

import Foundation


class iCloudFileManager {
    
    var containerUrl: URL? {
        return FileManager.default.url(forUbiquityContainerIdentifier: nil)?.appendingPathComponent("Documents")
    }
    
    func saveTransferedData(url: URL) {
        let fm = FileManager.default
        
        // check for container existence
        if let containerUrl = containerUrl {
            if !fm.fileExists(atPath: containerUrl.path, isDirectory: nil) {
                do {
                    print("Trying to create a directory...")
                    try fm.createDirectory(at: containerUrl, withIntermediateDirectories: true, attributes: nil)
                    print("  success")
                }
                catch {
                    print("error creating directory: \(error.localizedDescription)")
                }
            } else {
                print("Directory already exists.")
            }
            
            let targetUrl = containerUrl.appendingPathComponent(url.lastPathComponent)
            
            do {
                print("Trying to write text to file...")
                try fm.copyItem(at: url, to: targetUrl)
                print("  success")
            } catch {
                print("error writing file: \(error.localizedDescription)")
            }
        } else {
            print("`containerURL` is `nil`.")
        }
        
    }
}
