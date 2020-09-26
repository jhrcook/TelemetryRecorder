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
    
    func testMakingFile() {
        
        print("Begining test of iCloud file creation.")
        
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
            
            let documentURL = containerUrl.appendingPathComponent("some-text.txt")
            
            do {
                print("Trying to write text to file...")
                let str = String("Here is some text")
                try str.write(to: documentURL, atomically: true, encoding: .utf8)
                print("  success")
                print("file: \(documentURL.path)")
            } catch {
                print("error writing file: \(error.localizedDescription)")
            }
        } else {
            print("`containerURL` is `nil`.")
        }
    }
}
