module BookingCrud exposing (..)


import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, type_, placeholder, class, required)
import Html.Attributes as Attrs
import List exposing (any)

-- MODEL

type alias Booking =
    { id : Int
    , name : String
    , availability : Bool
    , date : String
    , time : String
    , attendees : String
    , purpose : String
    }

type alias Model =
    { bookings : List Booking
    , nextId : Int
    , editBookingId : Maybe Int
    , newBookerName : String
    , newDate : String
    , newTime : String
    , newAttendees : String
    , newPurpose : String
    , hasConflict : Bool
    }

init : Model
init =
    { bookings = []
        , nextId = 1
        , editBookingId = Nothing
        , newBookerName = ""
        , newDate = ""
        , newTime = ""
        , newAttendees = ""
        , newPurpose = ""
        , hasConflict = False
    }

-- UPDATE

hasConflict : Booking -> List Booking -> Bool
hasConflict newBooking existingBookings =
    let
        isConflictBooking booking =
            booking.date == newBooking.date && booking.time == newBooking.time
    in
    any isConflictBooking existingBookings

type Msg
    = AddOrUpdateBooking
    | UpdateBookerName String
    | UpdateDate String
    | UpdateTime String
    | UpdateAttendees String
    | UpdatePurpose String
    | DeleteBooking Int
    | StartEditBooking Booking
    | CancelEdit

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddOrUpdateBooking ->
            let 
                allFieldsComplete = model.newBookerName /= "" && model.newDate /= "" && model.newTime /= "" && model.newAttendees /= "" && model.newPurpose /= ""
                newBooking = { id = -1, name = model.newBookerName, availability = True, date = model.newDate, time = model.newTime, attendees = model.newAttendees, purpose = model.newPurpose }
                noConflicts = not (hasConflict newBooking model.bookings)
            in
            if allFieldsComplete && noConflicts then
                case model.editBookingId of
                    Just bookingId ->
                        let
                            updatedBookings = List.map (\booking -> if booking.id == bookingId then 
                                { booking 
                                    | name = model.newBookerName
                                    , date = model.newDate
                                    , time = model.newTime
                                    , attendees = model.newAttendees
                                    , purpose = model.newPurpose 
                                } else booking) model.bookings
                        in
                        { model 
                            | bookings = updatedBookings
                            , editBookingId = Nothing
                            , newBookerName = ""
                            , newDate = ""
                            , newTime = ""
                            , newAttendees = ""
                            , newPurpose = "" 
                        }

                    Nothing ->
                        { model
                            | bookings = model.bookings ++ [Booking model.nextId model.newBookerName True model.newDate model.newTime model.newAttendees model.newPurpose]
                            , nextId = model.nextId + 1
                            , newBookerName = ""
                            , newDate = ""
                            , newTime = ""
                            , newAttendees = ""
                            , newPurpose = ""
                        }
            else
                model

        UpdateBookerName newName ->
            { model | newBookerName = newName }

        UpdateDate newDate ->
            { model | newDate = newDate }

        UpdateTime newTime ->
            { model | newTime = newTime }

        UpdateAttendees newAttendees ->
            { model | newAttendees = newAttendees }

        UpdatePurpose newPurpose ->
            { model | newPurpose = newPurpose }

        DeleteBooking bookingId ->
            { model | bookings = List.filter (\booking -> booking.id /= bookingId) model.bookings }

        StartEditBooking booking ->
            { model
                | editBookingId = Just booking.id
                , newBookerName = booking.name
                , newDate = booking.date
                , newTime = booking.time
                , newAttendees = booking.attendees
                , newPurpose = booking.purpose
            }

        CancelEdit ->
            { model 
                | editBookingId = Nothing
                , newBookerName = ""
                , newDate = ""
                , newTime = ""
                , newAttendees = ""
                , newPurpose = "" 
            }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [  div [ class "main-input" ]
            [ h1 [] [text "Room 1"] 
            , div []
            [
                text "Booker Name"
                , br [] []
                , input 
                    [ placeholder "Booker Name", type_ "text", onInput UpdateBookerName, value model.newBookerName, required True ] []
            ]
            , div [] 
            [
                text "Date"
                , br [] []
                , input 
                    [ placeholder "Date"
                    , type_ "date"
                    , Attrs.min "2024-03-08"
                    , onInput UpdateDate
                    , value model.newDate
                    , required True ] []
            ]
            , div []
            [
                text "Time"
                , br [] []
                , select [ onInput UpdateTime, value model.newTime, required True ]
                    [
                        option [ value "9AM - 12PM" ] [ text "9AM - 12PM" ]
                        , option [ value "12PM - 3PM" ] [ text "12PM - 3PM" ]
                        , option [ value "3PM - 6PM" ] [ text "3PM - 6PM" ]
                    ]
            ]
            , div []
            [
                text "Num of attendees"
                , br [] []
                , select [ onInput UpdateAttendees, value model.newAttendees, required True ] 
                [
                    option [ value "10" ] [ text "10" ]
                    , option [ value "11" ] [ text "11" ]
                    , option [ value "12" ] [ text "12" ]
                    , option [ value "13" ] [ text "13" ]
                    , option [ value "14" ] [ text "14" ]
                    , option [ value "15" ] [ text "15" ]
                ]
            ]
            , div []
            [
                text "Purpose"
                , br [] []
                , input [ placeholder "Purpose", type_ "text", onInput UpdatePurpose, value model.newPurpose, required True ] []
            ]
            , div []
            [
                button [ onClick AddOrUpdateBooking ] [ text (if model.editBookingId == Nothing then "Add Booking" else "Update Booking") ]
            ]
            , if model.editBookingId /= Nothing then button [ onClick CancelEdit ] [ text "Cancel" ] else text ""
            ]
        , div [] (List.map (viewBooking model.editBookingId) model.bookings)
        ]

viewBooking : Maybe Int -> Booking -> Html Msg
viewBooking maybeEditBookingId booking =
    div []
        [ text ("Booking: " ++ booking.name 
            ++ ", Date: " ++ booking.date 
            ++ ", Time: " ++ booking.time
            ++ ", Num of attendees: " ++ booking.attendees
            ++ ", Purpose: " ++ booking.purpose ++ "")
            , button [ onClick (DeleteBooking booking.id) ] [ text "Delete" ]
            , if maybeEditBookingId /= Just booking.id then button [ onClick (StartEditBooking booking) ] [ text "Edit" ] else text ""
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
