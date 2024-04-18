int source();
int sink(int arg);

int fetch() { return source(); }

int main(int argc, char **argv) {
  int x = fetch();
  // CRAZY: If LIMIT >= 4, the error is no longer reported
#define LIMIT 3
  for (int i = 0; i < LIMIT; i += 1) {
    int z = 1;
  }
  return sink(x);
}
