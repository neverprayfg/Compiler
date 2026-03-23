#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <utility>
#include <stack>
class ReadText {
    public:
        std::string readtext(std::string &file_path) {
            std::ifstream file(file_path);
            if(!file.is_open()) {
                std::cerr << "Error: cannot open file " << file_path << std::endl;
                return "";
            }
            std::string text;
            std::string line;
            while(std::getline(file, line)) {
                text += line + '\n';
            }
            return text;
        }
};

enum class TokenType{
    INT,
    RETURN,
    ID,
    NUMBER,
    ASSIGN,
    PLUS,
    MINUS,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    SEMI
};

class Token {
    public:
        TokenType _type;
        std::string _word;
        Token(TokenType type, std::string word) : _type(type), _word(word) {}    
};

std::unordered_map<std::string, TokenType> keywords = {
    {"int", TokenType::INT},
    {"return", TokenType::RETURN}
};

bool isop(char c) {
    return c == '=' || c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')';
}

bool issemi(char c) {
    return c == ';';
}

std::vector<Token> tokenize(std::string text) {
    std::vector<Token> tokens;
    int pos = 0;
    while(pos < text.size()) {
        char c = text[pos];
        if(isspace(c)) {
            pos++;
            continue;
        }
        else if(isdigit(c)) {
            std::string number(1, c);
            while(pos + 1 < text.size() && isdigit(text[pos + 1])) {
                number += text[pos + 1];
                pos++;
            }
            tokens.push_back(Token(TokenType::NUMBER, number));
        }
        else if(isalpha(c)) {
            std::string word(1, c);
            while(pos + 1 < text.size() && isalpha(text[pos + 1])) {
                word += text[pos + 1];
                pos++;
            }
            if(keywords.find(word) != keywords.end()) {
                tokens.push_back(Token(keywords[word], word));
            }
            else {
                tokens.push_back(Token(TokenType::ID, word));
            }
        }
        else if(isop(c)) {
            switch(c) {
                case '=':
                    tokens.push_back(Token(TokenType::ASSIGN, std::string(1, c)));
                    break;
                case '+':
                    tokens.push_back(Token(TokenType::PLUS, std::string(1, c)));
                    break;
                case '-':
                    tokens.push_back(Token(TokenType::MINUS, std::string(1, c)));
                    break;
                case '*':
                    tokens.push_back(Token(TokenType::MUL, std::string(1, c)));
                    break;
                case '/':
                    tokens.push_back(Token(TokenType::DIV, std::string(1, c)));
                    break;
                case '(':
                    tokens.push_back(Token(TokenType::LPAREN, std::string(1, c)));
                    break;
                case ')':
                    tokens.push_back(Token(TokenType::RPAREN, std::string(1, c)));
                    break;
            }
        }
        else if(issemi(c)) {
            tokens.push_back(Token(TokenType::SEMI, std::string(1, c)));
        }
        ++pos;
    }
    return tokens;
}

class statement {
    public:
        std::vector<Token> _tokens;
        statement(std::vector<Token> tokens) : _tokens(tokens) {}
};



std::vector<statement> split(std::vector<Token> tokens, TokenType type) {
    std::vector<statement> states;
    for(int i = 0; i < tokens.size(); ++i) {
        int pos = i;
        std::vector<Token> state_tokens;
        while(pos< tokens.size() && tokens[pos]._type != type) {
            state_tokens.push_back(tokens[pos++]);
        }
        i = pos;
        states.push_back(statement(state_tokens));
    }
    return states;
}


enum class Associativity {
    LEFT,
    RIGHT,
    UNKNOWN
};

class CodeGenerator {
    private:
        int offset;
        std::unordered_map<std::string, int> table;
        std::unordered_map<TokenType, std::pair<Associativity, int>> precedence_table;
        std::string declaration(statement state) {
            // 技术债：假设declaration只有int类型
            std::string id = state._tokens[1]._word;
            if(table.find(id) != table.end()) {
                std::cerr << "Error: variable " << id << " is already declared" << std::endl;
                return "";
            }
            if(state._tokens[0]._type == TokenType::INT) {
                offset += 4;
                table[id] = -offset;
                return "sw $zero, " + std::to_string(-offset) + "($fp)\n";
            }
            return "";
        }

        std::string expression(statement state) {
            std::stack<Token> num_stack;
            std::stack<Token> op_stack;
            std::unordered_map<std::string, int> tmp_table = table;
            int sp_offset = 0;

            int counter = 0;
            std::string code = "";

            auto load = [&, this](Token &num, std::string reg) -> void {
                if(num._type == TokenType::NUMBER) {
                    code += "li " + reg + ", " + num._word + "\n";
                }
                else if(num._type == TokenType::ID) {
                    if(this->table.find(num._word) != this->table.end()) {
                        code += "lw " + reg + ", " + std::to_string(table[num._word]) + "($fp)\n";
                    }
                    else if(tmp_table.find(num._word) != tmp_table.end()) {
                        code += "lw " + reg + ", " + std::to_string(sp_offset - tmp_table[num._word]) + "($sp)\n";
                    }
                    else {
                        std::cerr << "Error: variable " << num._word << " is not declared" << std::endl;
                    }
                }
            };
            auto store = [&,this](Token &num, std::string reg) -> void {
                if(num._type == TokenType::ID) {
                    if(this->table.find(num._word) != this->table.end()) {
                        code += "sw " + reg + ", " + std::to_string(table[num._word]) + "($fp)\n";
                    }
                    else {
                        code += "addiu $sp, $sp, -4\n";
                        sp_offset += 4;
                        tmp_table[num._word] = sp_offset;
                        code += "sw " + reg + ", 0" + "($sp)\n";
                    }
                }
            };

            auto GenNewToken = [&counter]() -> Token {
                return Token(TokenType::ID, std::to_string(counter++) + "_tmp");
            };

            auto eval = [&](Token &num1, Token &num2, Token &op) -> Token {
                if(op._type == TokenType::ASSIGN) {
                    load(num2, "$t0");
                    store(num1, "$t0");
                    return num1;
                }
                else if(op._type == TokenType::PLUS || op._type == TokenType::MINUS || op._type == TokenType::MUL || op._type == TokenType::DIV) {
                    load(num1, "$t0");
                    load(num2, "$t1");  
                    if(op._type == TokenType::PLUS) {
                        code += "add $t0, $t0, $t1\n";
                    }
                    else if(op._type == TokenType::MINUS) {
                        code += "sub $t0, $t0, $t1\n";
                    }
                    else if(op._type == TokenType::MUL) {
                        code += "mul $t0, $t0, $t1\n";
                    }
                    else if(op._type == TokenType::DIV) {
                        code += "div $t0, $t0, $t1\n";
                    }
                    Token tmp_token = GenNewToken();
                    store(tmp_token, "$t0");
                    return tmp_token;
                }
            };

            auto pop_and_eval = [&]() {
                Token op = op_stack.top();
                op_stack.pop();
                Token num2 = num_stack.top();
                num_stack.pop();
                Token num1 = num_stack.top();
                num_stack.pop();
                Token tmp_token = eval(num1, num2, op);
                num_stack.push(tmp_token);
            };

            for(auto i: state._tokens) {
                if(i._type == TokenType::NUMBER || i._type == TokenType::ID) {
                    num_stack.push(i);
                }
                else if(i._type == TokenType::LPAREN) {
                    op_stack.push(i);
                }
                else if(i._type == TokenType::RPAREN) {
                    while(!op_stack.empty() && op_stack.top()._type != TokenType::LPAREN) {
                        pop_and_eval();
                    }
                    op_stack.pop();
                }
                else if(op_stack.empty()) {
                    op_stack.push(i);
                }
                else if(precedence_table[i._type].second > precedence_table[op_stack.top()._type].second) {
                    op_stack.push(i);
                }
                else if(precedence_table[i._type].second < precedence_table[op_stack.top()._type].second) {
                    while(!op_stack.empty() && precedence_table[i._type].second < precedence_table[op_stack.top()._type].second) {
                        pop_and_eval();
                    }
                    op_stack.push(i);
                }
                else if(precedence_table[i._type].second == precedence_table[op_stack.top()._type].second) {
                    if(precedence_table[i._type].first != Associativity::RIGHT) {
                        pop_and_eval();
                    }
                    op_stack.push(i);
                }
            }

            while(!op_stack.empty()) {
                pop_and_eval();
            }
            if(sp_offset > 0)
                code += "addiu $sp, $sp, " + std::to_string(sp_offset) + "\n";
            return code;
        }
        
        std::string return_statement(statement state) {
            // 技术债： 假设return返回的是一个ID
            std::string id = state._tokens[1]._word;
            if(table.find(id) == table.end()) {
                std::cerr << "Error: variable " << id << " is not declared" << std::endl;
                return "";
            }
            return "lw $v0, " + std::to_string(table[id]) + "($fp)\n";
        }
        
    public:
        CodeGenerator() : offset(0) {
            precedence_table = {
                {TokenType::LPAREN, {Associativity::UNKNOWN, 0}},
                {TokenType::ASSIGN, {Associativity::RIGHT, 1}},
                {TokenType::PLUS, {Associativity::LEFT, 2}},
                {TokenType::MINUS, {Associativity::LEFT, 2}},
                {TokenType::MUL, {Associativity::LEFT, 3}},
                {TokenType::DIV, {Associativity::LEFT, 3}}
            };
        }

        std::string GenCode(std::vector<statement> states) {
            std::string code;
            for(auto state : states) {
                std::string line;
                if(state._tokens.empty()) continue;
                else if(state._tokens[0]._type == TokenType::INT) {
                    line = declaration(state);
                }
                else if(state._tokens[0]._type == TokenType::RETURN) {
                    line = return_statement(state);
                }
                else {
                    line = expression(state);
                }
                code += line;
            }
            return code;
        }
};

int main(int argc, char* argv[]) {
    std::string file_path = argv[1];
    ReadText readtext;
    std::string text = readtext.readtext(file_path);
    std::vector<Token> tokens = tokenize(text);
    std::vector<statement> states = split(tokens, TokenType::SEMI);
    CodeGenerator codegenerator;
    std::string code = codegenerator.GenCode(states);
    std::cout << code << std::endl;
}