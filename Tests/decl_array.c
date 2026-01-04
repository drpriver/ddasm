// Test (6.7.6) array declarators

int global_arr[10];

int test_array_local(void) {
    int arr[5];
    arr[0] = 1;
    arr[4] = 5;
    return arr[0] + arr[4];
}

int test_array_param(int arr[10]) {
    return arr[0] + arr[9];
}

int sum_array(int* arr, int n) {
    int sum = 0;
    int i;
    for (i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}
int main(){
    int arr[10] = {1,2,3,4,5,6,7,8,9,10};
    test_array_local();
    test_array_param(arr);
    sum_array(arr, 10);
    return 0;
}
