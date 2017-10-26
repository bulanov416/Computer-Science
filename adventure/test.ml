type stuff = {
	name: string;
}

let bool change = true

let food : stuff = {name="salad"}

let changeFoodName (thingToChange: stuff)(newName: string) = 
	{thingToChange with name = newName}
;;

let go (newFoodName : string) = 
	changeFoodName food "pizza"
;;

go "pizza"
