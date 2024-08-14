for file in ./APS2/MonSample/*.aps
do
    echo "******************************************"
    echo $file ": " 
    type=$(./APS2/prologTerm $file | swipl -s ./APS2/prog.pl -g main 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "Type Checking OK."
    else
        echo "Type Checking error!"
    fi
    res=$(./APS2/eval $file)
    if [[ $res = "42" ]]; then
        echo "Eval Checking OK."    
    else
        echo "Eval Checking error! Result: $res " 
    fi
done