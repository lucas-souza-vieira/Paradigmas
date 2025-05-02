#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <sstream>

using namespace std;

int N; // Tamanho da grade
vector<vector<int>> values;
vector<vector<char>> regions;
map<char, vector<pair<int, int>>> region_cells;

// Direções: esquerda, direita, baixo, cima
const int dx[] = {-1, 1, 0, 0};
const int dy[] = {0, 0, -1, 1};

// Verifico se está dentro da matriz
bool in_bounds(int i, int j) {
    return i >= 0 && i < N && j >= 0 && j < N;
}

bool is_valid(int i, int j, int num) {
    char region_id = regions[i][j];

    // Número não pode repetir na mesma região
    for (auto [x, y] : region_cells[region_id]) {
        if (values[x][y] == num) return false;
    }

    // Células ortogonalmente adjacentes não podem ter o mesmo número
    for (int d = 0; d < 4; ++d) {
        int ni = i + dx[d], nj = j + dy[d];
        if (in_bounds(ni, nj) && values[ni][nj] == num)
            return false;
    }

    // Se está acima ou abaixo de célula da mesma região, deve respeitar ordem (superior > inferior)
    if (i > 0 && regions[i - 1][j] == region_id && values[i - 1][j] != 0){
        if (values[i - 1][j] <= num){
            return false;
        } 
    }

    if (i < N - 1 && regions[i + 1][j] == region_id && values[i + 1][j] != 0){
        if (values[i + 1][j] >= num){
            return false;
        } 
    }

    return true;
}

bool solve(int row = 0, int col = 0) {
    // Se chegar na ultima linha, retorna true pois deu tudo certo até lá
    if (row == N){
        return true;
    } 
    // Se chegar na ultima coluna, desce para a proxima linha, começando da primeira coluna
    if (col == N){
        return solve(row + 1, 0);
    } 
    // Se já tiver preenchido, vou para a proxima célula
    if (values[row][col] != 0){
        return solve(row, col + 1);
    }

    // Pego a região daquela celula
    char region_id = regions[row][col];
    // Pego a quantidade de celulas daquela região
    int region_size = region_cells[region_id].size();

    // Testo cada numero para aquela celula até funcionar todos
    for (int num = 1; num <= region_size; ++num) {
        // Se for valido, coloco aquele numero na celula
        if (is_valid(row, col, num)) {
            values[row][col] = num;
            // Se o proximo for valido, vou retornando pois preencheu tudo certo
            if (solve(row, col + 1)){
                return true;
            } 
            // Se não for valido, esse caminho não é possivel, volta desfazendo as mudanças
            values[row][col] = 0;
        }
    }

    return false;
}

void read_input() {
    string line;
    for (int i = 0; i < N; ++i) {
        getline(cin, line);
        stringstream ss(line);
        vector<int> row;
        int val;
        while (ss >> val) row.push_back(val);
        values.push_back(row);
    }

    for (int i = 0; i < N; ++i) {
        getline(cin, line);
        stringstream ss(line);
        vector<char> row;
        char r;
        int j = 0;
        while (ss >> r) {
            row.push_back(r);
            region_cells[r].emplace_back(i, j);
            ++j;
        }
        regions.push_back(row);
    }
}

void print_solution() {
    for (const auto& row : values) {
        for (int v : row)
            cout << v << " ";
        cout << '\n';
    }
}

int main() {
    cin >> N;
    cin.ignore(); 
    read_input();

    if (solve()) {
        print_solution();
    } else {
        cout << "Sem solução.\n";
    }

    return 0;
}
