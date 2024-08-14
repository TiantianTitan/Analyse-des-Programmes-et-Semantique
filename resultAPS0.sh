for file in ./APS0/MonSample/*.aps;
do
    echo "******************************************"
    echo $file ": " 
    type=$(./APS0/prologTerm $file | swipl -s ./APS0/prog.pl -g main 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "Type Checking OK."
    else
        echo "Type Checking error!"
    fi
    res=$(./APS0/eval $file)
    if [[ $res = "42" ]]; then
        echo "Eval Checking OK."    
    else
        echo "Eval Checking error! Result: $res " 
    
fi
done