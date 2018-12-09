port module Model exposing (Model, Entry, emptyModel, dcaSample, newEntry)

type alias Model =
    { entries : List Entry
    , current : Int
    , field : String
    , id : String
    , visibility : String
    }


type alias Entry =
    { description : String
    , answers : List String
    , selected : Int
    , correct : Int
    , completed : Bool
    , editing : Bool
    , id : String
    }


emptyModel : Model
emptyModel =
    { entries =
        [ newEntry "No Exam Loaded" [] 0 "default-id-0"
        ]
    , current = 0
    , visibility = "All"
    , field = ""
    , id = "default"
    }


newEntry : String -> List String -> Int -> String -> Entry
newEntry desc answers correct id =
    { description = desc
    , answers = answers
    , selected = -1
    , correct = correct
    , completed = False
    , editing = False
    , id = id
    }

dcaSampleId = "dca-sample"
dcaSample =
    [ newEntry "Which command is used to place an image into a registry?" [ "docker commit","docker tag","docker push","docker images", "docker pull" ] 2 "dca-sample-0"
    , newEntry "Which network allows Docker Trusted Registry components running on different nodes to communicate and replicate Docker Trusted Registry data?" [ "dtr-ol","dtr-hosts","dtr-br","dtr-vlan" ] 0 "dca-sample-1"
    , newEntry "Which of the following is not an endpoint exposed by Docker Trusted Registry that can be used to assess the health of a Docker Trusted Registry replica?" [ "/health","/nginx_status","/api/v0/meta/cluster_status","/replica_status" ] 2 "dca-sample-2"
    , newEntry "One of your developers is trying to push an image to the registry (dtr.example.com). The push fails with the error “denied: requested access to the resource is denied”. What should you verify the user has completed?"
          [
            "docker login -u <username> -p <password> dtr.example.com",
            "docker registry login -u username -p <password> dtr.example.com",
            "docker push <username>/<image:tag> dtr.example.com",
            "docker images login -u <username> -p <password> dtr.example.com"
          ] 0 "dca-sample-3"
    ]
