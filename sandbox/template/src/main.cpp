#include <iomanip>
#include <sstream>

class Matrix {

protected:
    double** matrix;
    int row_s;
    int column_s;

    void delete_matrix() {
        for (int i = 0; i < row_s; i++) {
            delete [] matrix[i];
        }
        delete [] matrix;
    }

    void copy_matrix(const Matrix& other) {
        row_s = other.row_s;
        column_s = other.column_s;
        matrix = new double* [row_s];
        for (int i = 0; i < row_s; i++) {
            matrix[i] = new double [column_s];
            for (int j = 0; j < column_s; j++) {
                matrix[i][j] = other.matrix[i][j];
            }
        }
    }

    double** create_matrix(int n, int m) {
        if (n < 1 || m < 1) {
            throw std::invalid_argument("Invalid sizes");
        }
        double** new_matrix = new double* [n];
        for (int i = 0; i < n; i++) {
            new_matrix[i] = new double [m]();
        }
        return new_matrix;
    }

public:
    //Конструктор по умолчанию
    Matrix(): row_s(1), column_s(1) {
        matrix = create_matrix(1, 1);
        matrix[0][0] = 0.0;
    }
    //Конструктор с параметрами
    Matrix(int n, int m): row_s(n), column_s(m) {
        matrix = create_matrix(n, m);
    }
    //Конструктор копирования
    Matrix(const Matrix& other) {
        copy_matrix(other);
    }
    //Деструктор
    virtual ~Matrix() {
        delete_matrix();
    }
    //Количество строк
    int row_size() const {
        return row_s;
    }
    //Количество столбцов
    int column_size() const {
        return column_s;
    }
    //Обращение к элементу матрицы
    double& operator()(const int& i, const int& j) {
        if (0 > i || i >= row_s || 0 > j || j >= column_s) {
            throw std::invalid_argument("Invalid row or column number");
        }
        return matrix[i][j];
    }
    //Клонирование другой матрицы
    Matrix& operator=(const Matrix& other) {
        if (this != &other) {
            delete_matrix();
            copy_matrix(other);
        }
        return *this;
    }
    //Cравнение матриц
    bool operator==(const Matrix& other) {
        if (row_s != other.row_s || column_s != other.column_s) {
            return 0;
        }
        for (int i = 0; i < row_s; i++) {
            for (int j = 0; j < column_s; j++) {
                if (matrix[i][j] != other.matrix[i][j]) return 0;            
            }
        }
        return 1;
    }
    //Произведение матриц
    Matrix operator*(const Matrix& other) const {
        if (column_s != other.row_s) {
            throw std::invalid_argument("The wrong matrix");
        }
        Matrix product(row_s, other.column_s);
        for (int i = 0; i < row_s; i++) {
            for (int j = 0; j < other.column_s; j++) {
                for (int k = 0; k < column_s; k++) {
                    product(i, j) += matrix[i][k] * other.matrix[k][j];
                }
            }
        }
        return product;
    }
    //Транспонирование матрицы
    Matrix& operator-() {
        Matrix temp(column_s, row_s);
        for (int i = 0; i < row_s; i++) {
            for (int j = 0; j < column_s; j++) {
                temp.matrix[j][i] = matrix[i][j];
            }
        }
        std::swap(matrix, temp.matrix);
        std::swap(row_s, temp.row_s);
        std::swap(column_s, temp.column_s);
        return *this;
    }
    //Сложение матриц
    Matrix operator+(const Matrix& other) const {
        if (row_s != other.row_s || column_s != other.column_s) {
            throw std::invalid_argument("The wrong matrix");
        }
        Matrix sum(row_s, column_s);
        for (int i = 0; i < row_s; i++) {
            for (int j = 0; j < column_s; j++) {
                sum(i, j) = matrix[i][j] + other.matrix[i][j];
            }
        }
        return sum;
    }
    //Умножение на скаляр
    Matrix operator*(const double& scalar) {
        Matrix product(row_s, column_s);
        for (int i = 0; i < row_s; i++) {
            for (int j = 0; j < column_s; j++) {
                product(i, j) = scalar * matrix[i][j];
            }
        }
        return product;
    }
    //Вывод матрицы
    friend std::ostream& operator<<(std::ostream& os, const Matrix& m) {
        int max_width = 1;
        for (int i = 0; i < m.row_s; i++) {
            for (int j = 0; j < m.column_s; j++) {
                std::ostringstream oss;
                oss << m.matrix[i][j];
                int width = static_cast<int>(oss.str().length());
                max_width = (width > max_width) ? width : max_width;
            }
        }

        max_width++;
        for (int i = 0; i < m.row_s; i++) {
            os << "| ";
            for (int j = 0; j < m.column_s; j++) {    
                os << std::setw(max_width) << std::left << m.matrix[i][j];
            }
            os << "|\n";
        }
        return os;
    }
};

class SquareMatrix: public Matrix {
    
public:
    //Конструктор по умолчанию
    SquareMatrix() = default;
    //Конструктор с параметром
    SquareMatrix(int sz): Matrix(sz, sz) {}
    //Стандартный конструктор копирования
    SquareMatrix(const SquareMatrix& other) = default;
    //Конструтор копирования родительского класса
    SquareMatrix(const Matrix& other): Matrix(other) {
        if (other.row_size() != other.column_size()) {
            throw std::invalid_argument("The matrix is not square");
        }
    }
    //Удаление небезопасного конструктора
    SquareMatrix(int n, int m) = delete;
    //Деструктора
    ~SquareMatrix() override = default;
    //Добавление новых методов
    int size() const { return row_s; }
    double get_determinant() {
        double det = 1;
        SquareMatrix temp(row_s);
        temp = *this;
        for (int i = 0; i < row_s; i++) {
            if (temp.matrix[i][i] == 0) {
                int j = i + 1;
                for (; j < row_s; j++) {
                    if (temp.matrix[j][i] != 0) {
                        std::swap(temp.matrix[j], temp.matrix[i]);
                        break;
                    }
                }
                if (j != row_s) {
                    det = -det;
                    i--;
                }
            }
            else {
                for (int j = i + 1; j < row_s; j++) {
                    double ratio = -temp.matrix[j][i] / temp.matrix[i][i];
                    for (int k = i; k < row_s; k++) {
                        temp.matrix[j][k] += ratio * temp.matrix[i][k];
                    }
                }
            }
        }
        for (int i = 0; i < row_s; i++) {
            det *= temp.matrix[i][i];
        }
        return det;
    }
};