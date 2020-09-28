//
//  PhoneToWatchConnectivityManager.swift
//  TelemetryRecorder
//
//  Created by Joshua on 9/28/20.
//

import Foundation
import SwiftUI
import WatchConnectivity

protocol PhoneToWatchConnectivityDelegate {
    func didRecieveNewDataFile(url: URL)
    func watchStateDidChange(session: WCSession)
}

class PhoneToWatchConnectivityManager: NSObject, WCSessionDelegate {
    
    private let session: WCSession
    
    var fileTransferDelegate: PhoneToWatchConnectivityDelegate? = nil
    
    init(session: WCSession = .default) {
        self.session = session
        super.init()
        if WCSession.isSupported() {
            session.delegate = self
            session.activate()
        }
    }
    
    func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error: Error?) {
        if let error = error {
            print("Error during Watch Connectivity activation: \(error.localizedDescription)")
        } else {
            print("Watch Connectivity session activated without error.")
        }
    }
}


// MARK: - Receiving data

extension PhoneToWatchConnectivityManager {
    func session(_ session: WCSession, didReceive file: WCSessionFile) {
        if let fileTransferDelegate = fileTransferDelegate {
            fileTransferDelegate.didRecieveNewDataFile(url: file.fileURL)
        }
    }
}



extension PhoneToWatchConnectivityManager {
    func sessionDidDeactivate(_ session: WCSession) {
        print("WC session deactivated.")
    }
    
    func sessionDidBecomeInactive(_ session: WCSession) {
        print("WC session deactivated")
    }
    
    func sessionWatchStateDidChange(_ session: WCSession) {
        print("WC session: Watch state did change.")
        if let d = fileTransferDelegate { d.watchStateDidChange(session: session) }
    }
    
    func sessionReachabilityDidChange(_ session: WCSession) {
        print("WC session: Watch reachability state did change.")
        if let d = fileTransferDelegate { d.watchStateDidChange(session: session) }
    }
}
