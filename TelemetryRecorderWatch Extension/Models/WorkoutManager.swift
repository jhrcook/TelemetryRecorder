//
//  WorkoutManager.swift
//  TelemetryRecorderWatch Extension
//
//  Created by Joshua on 9/26/20.
//

import Foundation
import HealthKit
import Combine

class WorkoutManager: NSObject, ObservableObject {
    
    var info: WorkoutInformation?
    
    /// - Tag: DeclareSessionBuilder
    let healthStore = HKHealthStore()
    var session: HKWorkoutSession!
    var builder: HKLiveWorkoutBuilder!
    
    // The app's workout state.
    var running: Bool = false
    
    
    /// - Tag: Publishers
    @Published var heartrate: Double = 0
    
    
    init(info: WorkoutInformation?) {
        self.info = info
    }
    
    
    // Request authorization to access HealthKit.
    func requestAuthorization() {
        // Requesting authorization.
        /// - Tag: RequestAuthorization
        // The quantity type to write to the health store.
        
        // The quantity types to read from the health store.
        let typesToRead: Set = [
            HKQuantityType.quantityType(forIdentifier: .heartRate)!,
            HKQuantityType.quantityType(forIdentifier: .heartRateVariabilitySDNN)!,
            HKQuantityType.quantityType(forIdentifier: .activeEnergyBurned)!
        ]
        
        // Request authorization for those quantity types.
        healthStore.requestAuthorization(toShare: [], read: typesToRead) { (success, error) in
            if let error = error {
                print("Error requesting data read/share authorization: \(error.localizedDescription)")
                return
            }
            print("Successfully requesting authoriation for data reading and sharing.")
        }
    }
    
    
    // Provide the workout configuration.
    func workoutConfiguration() -> HKWorkoutConfiguration {
        /// - Tag: WorkoutConfiguration
        let configuration = HKWorkoutConfiguration()
        configuration.activityType = .functionalStrengthTraining
        configuration.locationType = .indoor
        return configuration
    }
    
    
    // Start the workout.
    func startWorkout() {
        // Start the timer.
        running = true
        
        // Create the session and obtain the workout builder.
        /// - Tag: CreateWorkout
        do {
            session = try HKWorkoutSession(healthStore: healthStore, configuration: self.workoutConfiguration())
            builder = session.associatedWorkoutBuilder()
        } catch {
            // Handle any exceptions.
            print("Error creating workout: \(error.localizedDescription)")
            return
        }
        
        // Setup session and builder.
        session.delegate = self
        builder.delegate = self
        
        // Set the workout builder's data source.
        /// - Tag: SetDataSource
        builder.dataSource = HKLiveWorkoutDataSource(healthStore: healthStore,
                                                     workoutConfiguration: workoutConfiguration())
        
        // Start the workout session and begin data collection.
        /// - Tag: StartSession
        session.startActivity(with: Date())
        builder.beginCollection(withStart: Date()) { (success, error) in
            if let error = error {
                print("Error starting data collection: \(error.localizedDescription)")
                return
            }
            print("Workout data collection started successfully.")
        }
    }
    
    
    func endWorkout() {
        
        builder.endCollection(withEnd: Date()) { (success, error) in
            if let error = error {
                print("Data collection ended with error: \(error.localizedDescription)")
            } else if success {
                print("Data collection ended successfully.")
            } else {
                print("Data collection did not end successfully (but without error).")
            }
        }
        
        print("Ending workout session.")
        session.end()
        running = false
    }
    
    
    func resetWorkout() {
        // Does nothing at the moment.
    }
    
    
    // MARK: - Update the UI
    // Update the published values.
    func updateForStatistics(_ statistics: HKStatistics?) {
        guard let statistics = statistics else { return }
        
        DispatchQueue.main.async {
            switch statistics.quantityType {
            case HKQuantityType.quantityType(forIdentifier: .heartRate):
                /// - Tag: SetLabel
                let heartRateUnit = HKUnit.count().unitDivided(by: HKUnit.minute())
                let value = statistics.mostRecentQuantity()?.doubleValue(for: heartRateUnit)
                let roundedValue = Double( round( 1 * value! ) / 1 )
                self.heartrate = roundedValue
            default:
                return
            }
        }
    }
}


// MARK: - HKWorkoutSessionDelegate
extension WorkoutManager: HKWorkoutSessionDelegate {
    func workoutSession(_ workoutSession: HKWorkoutSession, didChangeTo toState: HKWorkoutSessionState,
                        from fromState: HKWorkoutSessionState, date: Date) {
        // Wait for the session to transition states before ending the builder.
        /// - Tag: SaveWorkout
        print("Workout session did change state: \(workoutStateDescription(toState))")
        if toState == .ended {
            self.builder.finishWorkout { (workout, error) in
                // Optionally display a workout summary to the user.
                if let error = error {
                    print("Builder did finish with error: \(error.localizedDescription)")
                }
                self.resetWorkout()
            }
        }
    }
    
    func workoutSession(_ workoutSession: HKWorkoutSession, didFailWithError error: Error) {
        print("Workout session failed: \(error.localizedDescription)")
    }
    
    func workoutStateDescription(_ state: HKWorkoutSessionState) -> String {
        switch state {
        case .ended:
            return "ended"
        case .notStarted:
            return "not started"
        case .paused:
            return "paused"
        case.prepared:
            return "prepared"
        case .running:
            return "running"
        case .stopped:
            return "stopped"
        default:
            return "(unknown state)"
        }
    }
}


// MARK: - HKLiveWorkoutBuilderDelegate
extension WorkoutManager: HKLiveWorkoutBuilderDelegate {
    func workoutBuilderDidCollectEvent(_ workoutBuilder: HKLiveWorkoutBuilder) {
        // Does nothing for now.
    }
    
    func workoutBuilder(_ workoutBuilder: HKLiveWorkoutBuilder, didCollectDataOf collectedTypes: Set<HKSampleType>) {
        for type in collectedTypes {
            guard let quantityType = type as? HKQuantityType else {
                return // Nothing to do.
            }
            
            /// - Tag: GetStatistics
            let statistics = workoutBuilder.statistics(for: quantityType)
            updateForStatistics(statistics)
            
            // TODO: Something with the `statistics`.
        }
    }
}
