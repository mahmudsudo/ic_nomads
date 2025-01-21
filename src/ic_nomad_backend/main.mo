import Array "mo:base/Array";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Result "mo:base/Result";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Iter "mo:base/Iter";

actor ICNomads {
    // Types
    type UserId = Principal;
    type CompanyId = Principal;
    
    public type UserView = {
        id: UserId;
        username: Text;
        email: Text;
        xpPoints: Nat;
        level: Nat;
        isCompany: Bool;
        createdAt: Int;
        activities: [ActivityView];
    };

    private type UserData = {
        id: UserId;
        var username: Text;
        var email: Text;
        var xpPoints: Nat;
        var level: Nat;
        isCompany: Bool;
        createdAt: Time.Time;
        activities: Buffer.Buffer<Activity>;
    };

    public type CompanyView = {
        id: CompanyId;
        name: Text;
        description: Text;
        website: ?Text;
        postedBounties: [BountyId];
    };

    private type CompanyData = {
        id: CompanyId;
        var name: Text;
        var description: Text;
        var website: ?Text;
        postedBounties: Buffer.Buffer<BountyId>;
    };

    type BountyId = Nat;
     type MeetupId = Nat;

    public type BountyView = {
        id: BountyId;
        companyId: CompanyId;
        title: Text;
        description: Text;
        reward: Nat;
        status: BountyStatus;
        createdAt: Int;
        deadline: Int;
        submissions: [SubmissionView];
    };

    private type BountyData = {
        id: BountyId;
        companyId: CompanyId;
        var title: Text;
        var description: Text;
        var reward: Nat;
        var status: BountyStatus;
        createdAt: Time.Time;
        deadline: Time.Time;
        submissions: Buffer.Buffer<Submission>;
    };

    public type BountyStatus = {
        #Open;
        #InProgress;
        #Completed;
        #Cancelled;
    };

    public type SubmissionView = {
        userId: UserId;
        bountyId: BountyId;
        content: Text;
        submittedAt: Int;
        status: SubmissionStatus;
    };

    private type Submission = {
        userId: UserId;
        bountyId: BountyId;
        content: Text;
        submittedAt: Time.Time;
        var status: SubmissionStatus;
    };

    public type SubmissionStatus = {
        #Pending;
        #Accepted;
        #Rejected;
    };

    public type ActivityView = {
        activityType: ActivityType;
        timestamp: Int;
        points: Nat;
    };

    private type Activity = {
        activityType: ActivityType;
        timestamp: Time.Time;
        points: Nat;
    };

    public type ActivityType = {
        #TelegramChat;
        #TwitterPost;
        #BountySubmission;
        #HashtagUsed: Text;
    };

    public type MeetUp = {
        id: Nat;
        topic: Text;
        time: Nat;
        date: Text;
        createdAt: Time.Time;
        status: meetupState;
        creator : Principal;
    };
    public type meetupState= {
        #inProgress;
        #upcoming;
        #completed;
    };
    public type meetupPayload = {
        topic: Text;
        time: Nat;
        date: Text;
        status: meetupState;
    };

    // State Variables
    private stable var nextBountyId: Nat = 0;
    private stable var nextMeetupId: Nat= 0;
    private var users = HashMap.HashMap<UserId, UserData>(0, Principal.equal, Principal.hash);
    private var companies = HashMap.HashMap<CompanyId, CompanyData>(0, Principal.equal, Principal.hash);
    private var bounties = HashMap.HashMap<BountyId, BountyData>(0, Nat.equal, Hash.hash);
    private var meetups = HashMap.HashMap<MeetupId, MeetUp>(0, Nat.equal, Hash.hash);
    // Conversion functions
    private func userToUserView(user: UserData) : UserView {
        {
            id = user.id;
            username = user.username;
            email = user.email;
            xpPoints = user.xpPoints;
            level = user.level;
            isCompany = user.isCompany;
            createdAt = Time.now();
            activities = Buffer.toArray(user.activities);
        }
    };

    private func companyToCompanyView(company: CompanyData) : CompanyView {
        {
            id = company.id;
            name = company.name;
            description = company.description;
            website = company.website;
            postedBounties = Buffer.toArray(company.postedBounties);
        }
    };

    private func submissionToSubmissionView(submission: Submission) : SubmissionView {
        {
            userId = submission.userId;
            bountyId = submission.bountyId;
            content = submission.content;
            submittedAt = submission.submittedAt;
            status = submission.status;
        }
    };

    private func bountyToBountyView(bounty: BountyData) : BountyView {
        {
            id = bounty.id;
            companyId = bounty.companyId;
            title = bounty.title;
            description = bounty.description;
            reward = bounty.reward;
            status = bounty.status;
            createdAt = bounty.createdAt;
            deadline = bounty.deadline;
            submissions = Buffer.toArray(Buffer.map<Submission, SubmissionView>(bounty.submissions, submissionToSubmissionView));
        }
    };

    //Meetup management
    public shared({caller}) func addMeetUp(payload: meetupPayload): async Result.Result<(), Text>{
         let meetupId = nextMeetupId;
        nextMeetupId += 1;
        let newMeetup: MeetUp = {
        id=  meetupId;
        topic =  payload.topic;
        time =  payload.time;
        date =  payload.date;
        createdAt =  Time.now();
        status =  #upcoming;
        creator = caller;
        };
        meetups.put(meetupId, newMeetup);
         #ok(());
    };
  public shared ({caller}) func editMeetup(meetupId: MeetupId, payload: meetupPayload): async Result.Result<(), Text>  {
     let oldMeetup: ?MeetUp = meetups.get(meetupId);
     switch(oldMeetup){
        case(null){#err("meetup not found")}; //meetup doesnt exist
        case(?currentMeetup){
            if(currentMeetup.creator != caller){
              #err(("you are not the func creator"));
            }else{
                let updatedMeetup: MeetUp = {
                topic = payload.topic;
                time = payload.time;
                date = payload.date;
                status = payload.status;
                createdAt = currentMeetup.createdAt;
                id = currentMeetup.id;
                creator = currentMeetup.creator;
                };
                meetups.put(meetupId, updatedMeetup);
                #ok(());
            }
        }
     };
};
    public query func getAllMeetups():async [MeetUp] {
        Iter.toArray(meetups.vals());
    };
    public query func getMeetup(id: Nat): async ?MeetUp{
        return meetups.get(id);
    };
    public shared({caller}) func deleteMeetup(meetupId: MeetupId): async Result.Result<(), Text> {
    switch (meetups.get(meetupId)) {
        case null {
            #err("Meetup not found");
        };
        case (?meetup) {
            if (meetup.creator != caller) {
                #err("Only the creator of the meetup can delete it");
            } else {
                // Remove the meetup
               ignore meetups.remove(meetupId);
                #ok(());
            }
        };
    };
};


    // User Management
    public shared({caller}) func registerUser(username: Text, email: Text) : async Result.Result<(), Text> {
        
        
        switch (users.get(caller)) {
            case (?_) { #err("User already exists") };
            case null {
                let newUser: UserData = {
                    id = caller;
                    var username = username;
                    var email = email;
                    var xpPoints = 0;
                    var level = 1;
                    isCompany = false;
                    createdAt = Time.now();
                    activities = Buffer.Buffer<Activity>(0);
                };
                users.put(caller, newUser);
                #ok(());
            };
        };
    };

    public shared({caller}) func registerCompany(name: Text, description: Text, website: ?Text) : async Result.Result<(), Text> {
        
        
        switch (companies.get(caller)) {
            case (?_) { #err("Company already exists") };
            case null {
                let newCompany: CompanyData = {
                    id = caller;
                    var name = name;
                    var description = description;
                    var website = website;
                    postedBounties = Buffer.Buffer<BountyId>(0);
                };
                companies.put(caller, newCompany);
                #ok(());
            };
        };
    };

public query func getUserProfile(userId: UserId) : async ?UserView {
    switch (users.get(userId)) { //gets the detailed view of the user profile
        case null { null };
        case (?user) { ?userToUserView(user) };
    };
};
public shared({caller}) func deleteUser(userId: UserId): async Result.Result<(), Text> {
    switch (users.get(userId)) {
        case null {
            #err("User not found");
        };
        case (?user) {
            if (userId != caller) {
                #err("Only the user themselves can delete their account");
            } else {
                // Remove the user
               ignore users.remove(userId);
                #ok(());
            }
        };
    };
};


    // Bounty Management
    public shared({caller}) func createBounty(
        title: Text,
        description: Text,
        reward: Nat,
        deadline: Int
    ) : async Result.Result<BountyId, Text> {
        
        
        switch (companies.get(caller)) {
            case null { #err("Only registered companies can create bounties") };
            case (?company) {
                let bountyId = nextBountyId;
                nextBountyId += 1;
                
                let newBounty: BountyData = {
                    id = bountyId;
                    companyId = caller;
                    var title = title;
                    var description = description;
                    var reward = reward;
                    var status = #Open;
                    createdAt = Time.now();
                    deadline = deadline;
                    submissions = Buffer.Buffer<Submission>(0);
                };
                
                bounties.put(bountyId, newBounty);
                company.postedBounties.add(bountyId);
                #ok(bountyId);
            };
        };
    };

    public query func getBounty(bountyId: BountyId) : async ?BountyView {
        switch (bounties.get(bountyId)) {
            case null { null };
            case (?bounty) { ?bountyToBountyView(bounty) };
        };
    };

    public query func getAllBounties() : async [BountyView] {
        let bountiesArray = Buffer.Buffer<BountyView>(0);
        for ((_, bounty) in bounties.entries()) {
            bountiesArray.add(bountyToBountyView(bounty));
        };
        Buffer.toArray(bountiesArray);
    };

    // Submission Management
    public shared({caller}) func submitBounty(bountyId: BountyId, content: Text) : async Result.Result<(), Text> {
        
        
        switch (bounties.get(bountyId)) {
            case null { #err("Bounty not found") };
            case (?bounty) {
                if (bounty.status != #Open) {
                    return #err("Bounty is not open for submissions");
                };

                let submission: Submission = {
                    userId = caller;
                    bountyId = bountyId;
                    content = content;
                    submittedAt = Time.now();
                    var status = #Pending;
                };
                
                bounty.submissions.add(submission);
                switch (await addActivity(caller, #BountySubmission, 50)) {
                    case (#ok(_)) { #ok(()) };
                    case (#err(e)) { #err(e) };
                };
            };
        };
    };

    // Activity and XP Management
    public shared func addActivity(userId: UserId, activityType: ActivityType, points: Nat) : async Result.Result<(), Text> {
        switch (users.get(userId)) {
            case null { #err("User not found") };
            case (?user) {
                let activity: Activity = {
                    activityType = activityType;
                    timestamp = Time.now();
                    points = points;
                };
                
                user.activities.add(activity);
                user.xpPoints := user.xpPoints + points;
                user.level := user.xpPoints / 1000 + 1;
                #ok(());
            };
        };
    };

    // Leaderboard
    public query func getLeaderboard() : async [(Principal, Nat, Nat)] {
        let leaderboard = Buffer.Buffer<(Principal, Nat, Nat)>(0);
        for ((userId, user) in users.entries()) {
            leaderboard.add((userId, user.xpPoints, user.level));
        };
        
        let leaderboardArray = Buffer.toArray(leaderboard);
        Array.sort<(Principal, Nat, Nat)>(leaderboardArray, func(a, b) {
            if (a.1 > b.1) { #less }
            else if (a.1 < b.1) { #greater }
            else { #equal }
        })
    };

    // Profile Management
    public query({caller}) func getProfile() : async Result.Result<UserView, Text> {
        
        switch (users.get(caller)) {
            case null { #err("User not found") };
            case (?user) { #ok(userToUserView(user)) };
        };
    };

    public shared({caller}) func updateProfile(username: Text, email: Text) : async Result.Result<(), Text> {
        
        switch (users.get(caller)) {
            case null { #err("User not found") };
            case (?user) {
                user.username := username;
                user.email := email;
                #ok(());
            };
        };
    };
};