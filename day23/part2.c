#include <stdio.h>
#include <stdlib.h>

#define million 1000000
#define iterations 10000000

struct node {
    int data;
    int key;
    struct node *next;
};

// don't use this
int maximum(int len, int arr[]) {
    int max_so_far = arr[0];
    for (int i=1; i<len; i++) {
        if (max_so_far <= arr[i]) {
            max_so_far = arr[i];
        }
    }
    return max_so_far;
}

struct node *make_node(int key, int data) {
    struct node *link = (struct node *) malloc(sizeof(struct node));
    link->key = key;
    link->data = data;
    link->next = NULL;
    return link;
}

struct node *make_circular_list(int len, int starting_nums[]) {
    struct node *head = make_node(0, starting_nums[0]);
    struct node *current = head;

    int i;
    for (i=1; i<len; i++) {
        current->next = make_node(i, starting_nums[i]);
        current = current->next;
    }

    int m = 10;
    for (; i<million; i++) {
        current->next = make_node(i, m);
        current = current->next;
        m++;
    }

    current->next = head;

    return head;
}

int elem(int x, int len, struct node *arr[]) {
    for (int i=0; i<len; i++) {
        if (arr[i]->data == x) {
            return 1;
        }
    }
    return 0;
}

struct node *move(struct node *nodes[], struct node *cups) {
    struct node *curr = cups;

    // copy the clockwise three to an array
    struct node *lifted[3] = {curr->next, curr->next->next, curr->next->next->next};
    /*
    for (int i=0; i<3; i++) {
        printf("lifted %i: %i\n", i, lifted[i]->data);
    }
    */

    // delete the clockwise three from the linked list
    curr->next = curr->next->next->next->next;

    // pick destination
    int destVal = curr->data - 1;
    while (destVal <= 0 || destVal == lifted[0]->data || destVal == lifted[1]->data || destVal == lifted[2]->data) {
        destVal--;
        if (destVal <= 0) {
            destVal = million;
        }
    }
    //printf("dest: %i", destVal);

    // find the actual destination node
    struct node *dest = nodes[destVal];
    /*
    struct node *dest = curr->next;
    // can we avoid this computation? this is the most expensive thing we do
    while (dest->data != destVal) {
        dest = dest->next;
    }
    printf("%i, next: %i\n", dest->data, dest->next->data);
    */

    // insert the clockwise three after the destination
    struct node *temp = dest->next;
    for (int i=0; i<3; i++) {
        dest->next = lifted[i];
        dest = dest->next;
    }
    dest->next = temp;

    return curr->next;
}

void print_list(struct node *head) {
    struct node *curr = head->next;
    printf("%i ", head->data);
    while (curr != head) {
        printf("%i ", curr->data);
        curr = curr->next;
    }
    printf("\n");
}

int main() {
    int len = 9;
    int test_start[] = {3, 8, 9, 1, 2, 5, 4, 6, 7};
    int start[]      = {6, 2, 4, 3, 9, 7, 1, 5, 8};

    struct node *head = make_circular_list(len, start);

    // make an instant access time thingie
    struct node *nodes[million + 1];
    struct node *curr = head->next;
    nodes[head->data] = head;
    while (curr != head) {
        nodes[curr->data] = curr;
        curr = curr->next;
    }

    // solve
    curr = head;
    for (int n=0; n<iterations; n++) {
        curr = move(nodes, curr);
        //print_list(curr);
        if (n % 1000000 == 0) {
            printf("%i\n", n);
        }
    }

    while (curr->data != 1) {
        curr = curr->next;
    }

    int64_t answer = ((int64_t) nodes[1]->next->data) * ((int64_t) nodes[1]->next->next->data);
    printf("Answer: %li\n", answer);

    return 0;
}
