;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SIMULADOR DE MAQUINA ENIGMA M3 + JOGO TIPO FORCA;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Intruções
(display "INSTRUÇÕES:") (newline)
(display "*Para iniciar o jogo escreve (inicia_jogo);") (newline)
(display "*No jogo insere todos(as) carateres(frases) entre aspas;") (newline)
(display "*Para parar o jogo introduza f_break;") (newline)
(display "*Para intruções mais completas escrever (intro_comp).") (newline)

(define intro_comp
  (lambda ()
    (display "*********INSTRUÇÕES COMPLETAS:********") (newline)
    (display "JOGO:") (newline)
    (display "  (inicia_jogo)                      : para iniciar o jogo.") (newline)
    (display "CRIADORES:") (newline)
    (display "  (configs_creator a b c d e)        : cria a própria configuração - rotors_order(a) ring_settings(b) rotors_startpoint(c) plugboard(d) reflector(e);") (newline)
    (display "  (rotors_order x y z)               : coloca 3 rotors pela ordem a serem utilizados;") (newline)
    (display "  (ring_settings x y z)              : usa 3 carateres para definir o ponto inicial dos rotors;") (newline)
    (display "  (plugboard_reflexor e1 ... e13)    : recebe 13 listas de strings de 2 elementos para criar plugboards e reflexores;") (newline)
    (display "  (r_rotors e1 ... e26               : recebe 26 listas de strings de 2 elementos para criar rotors;") (newline)
    (display "  (ref_plugboard_pair x y)           : cria um par para reflexor/plugboard;") (newline)
    (display "  (rotors_startpoint x y z)          : define posição da letra inicial escolhida em ring_settings.") (newline)
    (display "SELETORES:") (newline)
    (display "  (s_rotor1/2/3 config)              : seleciona o rotor 1/2/3 da configuração inserida;") (newline)
    (display "  (s_plugboard config)               : seleciona a plugboard da configuração inserida;") (newline)
    (display "  (s_reflexor config)                : seleciona o reflexor da configuração inserida;") (newline)
    (display "  (s_rset1/2/3 config)               : seleciona a ring_setting 1/2/3 da configuração inserida;") (newline)
    (display "  (s_rsp11/2/3 config)               : seleciona o rotor_startpoint 1/2/3 da configuração inserida.") (newline)
    (display "INVERSOR:") (newline)
    (display "  (inv_rotor rotor)                  : inversor de rotor.") (newline)
    (display "GERADOR RANDOM:") (newline)
    (display "  (r_generator x)                    : 'random generator' de rotors(x=1) plugboard(x=2), refletor(x=3), ring_settings(x=4), rotors_startpoint(x=5);") (newline)
    (display "   r_config_creator                  : gera uma configuração random cada vez que corre o programa.") (newline)
    (display "CODIFICADOR:") (newline)
    (display "  (r_code mensagem_entre_aspas)      : codifica a mensagem com a config de r_config_creator;") (newline)
    (display "  (code config message setting show) : codica uma mensagem -> config(configuração desejada) setting('c para codificar, 'd para descodificar) show('y para display).") (newline)
    (display "**************************************") (newline)))


;letras possiveis de se usar : A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 
(define configs_creator ;ex: ((rotor1 rotor2 rotor3) "AAA" (1 1 1) plugboard1 reflector1)
  (lambda (rotors_order ring_settings rotors_startpoint plugboard reflector)   
    (list rotors_order ring_settings rotors_startpoint plugboard reflector)))

;posição relativa dos rotors ex: 3 1 2
(define rotors_order
  (lambda (fst scn thrd)
    (list fst scn thrd)))

;posição de relação letra-ligação ex: F B G
(define ring_settings
  (lambda (fst scn thrd)
    (string fst scn thrd)))

;lista de pares do plugboard e reflexores
(define plugboard_reflexor
  (lambda (e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13)
    (append (list e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13) 
            (map (lambda(x) (list->string (list (cadr (string->list x)) (car (string->list x))))) 
                 (list e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13)))))

;lista de pares dos rotors e reflector
(define r_rotors
  (lambda (e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 
              e14 e15 e16 e17 e18 e19 e20 e21 e22 e23 e24 e25 e26)
    (list e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13
          e14 e15 e16 e17 e18 e19 e20 e21 e22 e23 e24 e25 e26)))

;pares do plugboard/plugboard
(define ref_plugboard_pair
  (lambda (ele1 ele2)
    (string ele1 ele2)))

;ponto numérico de inicio dos rotors
(define rotors_startpoint
  (lambda (rp1 rp2 rp3)
    (list rp1 rp2 rp3)))

;seletores
(define s_rotor1
  (lambda (config)
    (caar config)))

(define s_rotor2
  (lambda (config)
    (cadar config)))

(define s_rotor3
  (lambda (config)
    (caddar config)))

(define s_plugboard
  (lambda (config)
    (cadddr config)))

(define s_reflector
  (lambda (config)
    (cadr (cdddr config))))

;seletores ring_settings
(define s_rset1
  (lambda (config)
    (string-ref (cadr config) 0)))

(define s_rset2
  (lambda (config)
    (string-ref (cadr config) 1)))

(define s_rset3
  (lambda (config)
    (string-ref (cadr config) 2)))

;seletore rotors_startpoint
(define s_rsp1
  (lambda (config)
    (caaddr config)))

(define s_rsp2
  (lambda (config)
    (car (cdaddr config))))

(define s_rsp3
  (lambda (config)
    (caddr (caddr config))))

;inversor dos rotors
(define inv_rotor
  (lambda (rotor)
    (letrec ((aux
              (lambda (i nrotor)
                (if (= i 26)
                    nrotor
                    (aux (add1 i) (append nrotor (list (list->string (list (cadr (string->list (list-ref rotor i))) (car (string->list (list-ref rotor i))))))))))))
      (aux 0 ()))))

;"random generator" de rotors(1) - 26, plugboard(2) - 13, refletor(3) - 13, 
;                      ring_settings(4) - string de 3 letras, 
;                      rotors_startpoint(5) 
(define r_generator 
  (lambda (def)
    (cond ((= def 1) (r_generator_aux1 def 
                                       (vector->list (make-vector 26 -1)) ;vetor de introdução de randoms para rotors 
                                       (string->list (string-copy "ABCDEFGHIJKLMNOPQRSTUVWYXZ")))) ;usada para rotors
          ((or (= def 2) (= def 3)) (r_generator_aux1 def 
                                                      (vector->list (make-vector 13 -1)) ;vetores de introdução de randoms para plugboards e reflexores
                                                      (vector->list (make-vector 13 -1)))) ;usada para plugboards e reflexores
          ((= def 4) (r_generator_aux1 def () ()))
          ((= def 5) (r_generator_aux1 def () ()))
          (else (display "erro_input errado")))))

(define r_generator_aux1 ;lista1 - lista de randoms ;lista2 - lista de letras; ct - limitador de iterações
  (lambda (def lista1 lista2)
    (cond ((= def 1) 
           (map (lambda (x) (list->string x)) (r_generator_aux2 0 (random 26) lista1 lista2 26))) 
          ((or (= def 2) (= def 3))  
           (r_generator_aux4 (map (lambda (x) (list->string x)) (r_generator_aux3 0 (random 26) (random 26) lista1 lista2 (vector->list (make-vector 13 -1)) 13))))
          ((= def 4) (list->string (list (integer->char (+ 65 (random 26))) (integer->char (+ 65 (random 26))) (integer->char (+ 65 (random 26))))))
          ((= def 5) (list (add1 (random 26)) (add1 (random 26)) (add1 (random 26)))))))

(define r_generator_aux2
  (lambda (i rnd lista1 lista2 ct) 
    (if (= i ct)
        lista2
        (if (or (boolean? (member rnd lista1)) 
                (boolean? (member (+ 65 rnd) (map (lambda (x) (char->integer (if (pair? x)(car x)x))) lista2))))
            (and (set-car! (list-tail lista2 i) (list (list-ref lista2 i) (integer->char (+ 65 rnd))))
                 (set-car! (list-tail lista1 i) rnd)
                 (r_generator_aux2 (add1 i) (random 26) lista1 lista2 ct))
            (r_generator_aux2 i (random 26) lista1 lista2 ct)))))

(define r_generator_aux3
  (lambda (i rnd1 rnd2 lista1 lista2 lista3 ct) 
    (if (= i ct)
        lista3
        (if (and (boolean? (member rnd1 lista1))
                 (boolean? (member rnd1 lista2))
                 (boolean? (member rnd2 lista1))
                 (boolean? (member rnd2 lista2))
                 (not (= rnd1 rnd2)))
            (and (set-car! (list-tail lista3 i) (list (integer->char (+ 65 rnd1)) (integer->char (+ 65 rnd2))))
                 (set-car! (list-tail lista1 i) rnd1)
                 (set-car! (list-tail lista2 i) rnd2)
                 (r_generator_aux3 (add1 i) (random 26) (random 26) lista1 lista2 lista3 ct))
            (r_generator_aux3 i (random 26) (random 26) lista1 lista2 lista3 ct)))))

(define r_generator_aux4
  (lambda (lista1)
    (plugboard_reflexor (list-ref lista1 0) (list-ref lista1 5) (list-ref lista1 9) 
                        (list-ref lista1 1) (list-ref lista1 6) (list-ref lista1 10) 
                        (list-ref lista1 2) (list-ref lista1 7) (list-ref lista1 11) 
                        (list-ref lista1 3) (list-ref lista1 8) (list-ref lista1 12) 
                        (list-ref lista1 4))))

;criador de configuração aleatória
(define r_config_creator
  (configs_creator (list (r_generator 1) (r_generator 1) (r_generator 1))
                   (r_generator 4)
                   (r_generator 5)
                   (r_generator 2)
                   (r_generator 3)))
  
;controlo de ponto de partida! - tudo pronto para iniciar (des)codificação
(define i_rotors!
  (lambda (config)
    (letrec ((aux0 ;altera constituinte do par de ref 0
              (lambda (c v rotor)
                (if (or(> c 25) (zero? v))
                    (aux1 0 v rotor)
                    (if (> (+ v (char->integer (string-ref (list-ref rotor c) 0))) (char->integer #\Z))
                        (and (string-set! (list-ref rotor c) 0 (integer->char (+ v (char->integer (car (string->list (list-ref rotor c)))) -26)))
                             (aux0 (add1 c) v rotor))
                        (if (< (+ v (char->integer (string-ref (list-ref rotor c) 0))) (char->integer #\A))
                            (and (string-set! (list-ref rotor c) 0 (integer->char (+ v (char->integer (car (string->list (list-ref rotor c)))) 26)))
                                 (aux0 (add1 c) v rotor))
                            (and (string-set! (list-ref rotor c) 0 (integer->char (+ v (char->integer (car (string->list (list-ref rotor c)))))))
                                 (aux0 (add1 c) v rotor)))))))
             (aux1 ;altera constituinte do par de ref 1
              (lambda (c v rotor)
                (if (or(> c 25) (zero? v))
                    ()
                    (if (> (+ v (char->integer (string-ref (list-ref rotor c) 1))) (char->integer #\Z))
                        (and (string-set! (list-ref rotor c) 1 (integer->char (+ v (char->integer (cadr (string->list (list-ref rotor c)))) -26)))
                             (aux1 (add1 c) v rotor))
                        (if (< (+ v (char->integer (string-ref (list-ref rotor c) 1))) (char->integer #\A))
                            (and (string-set! (list-ref rotor c) 1 (integer->char (+ v (char->integer (cadr (string->list (list-ref rotor c)))) 26)))
                                 (aux1 (add1 c) v rotor))
                            (and (string-set! (list-ref rotor c) 1 (integer->char (+ v (char->integer (cadr (string->list (list-ref rotor c)))))))
                                 (aux1 (add1 c) v rotor))))))))
      (aux0 0 (- (char->integer (s_rset1 config)) (char->integer (string-ref (list-ref (s_rotor1 config) (sub1 (s_rsp1 config))) 0))) (s_rotor1 config)) ;diferença entre carater definido como posicao inicial e 
      (aux0 0 (- (char->integer (s_rset2 config)) (char->integer (string-ref (list-ref (s_rotor2 config) (sub1 (s_rsp2 config))) 0))) (s_rotor2 config)) ;o carater que devia estar nessa posição
      (aux0 0 (- (char->integer (s_rset3 config)) (char->integer (string-ref (list-ref (s_rotor3 config) (sub1 (s_rsp3 config))) 0))) (s_rotor3 config))))) 

;atualizador dos estados dos rotors!
(define a_rotors!
  (lambda (config mostrador posici) ;mostrador indica o avanço relativo a posição inicial
    (i_rotors! (configs_creator (car config) posici mostrador (s_plugboard config) (s_reflector config)))))

;codifica e descodifica mensagens
(define r_code
  (lambda (message)
    (code r_config_creator message 'c 'y)))

(define code
  (lambda (config message setting show)
    (if (string? message)
        (princ_code config message setting (string-copy "AAA") show)
        #f)))

(define ini_code
  (lambda (message setting)
    (if (equal? setting 'c)
        (and (display "MESSAGE: ") (display message) (newline))
        (if (equal? setting 'd)
            (and (display "CODE:    ") (display message) (newline))
            #f))))

(define princ_code
  (lambda (config message setting posici show) 
    (if (equal? show 'y) (ini_code message setting)) ;mensagem inicial
    (i_rotors! config)         ;coloca rotors prontos a começar
    (string-set! posici 0 (car (list-ref (map (lambda (x) (string->list x)) (s_rotor1 config)) 0)))
    (string-set! posici 1 (car (list-ref (map (lambda (x) (string->list x)) (s_rotor2 config)) 0)))
    (string-set! posici 2 (car (list-ref (map (lambda (x) (string->list x)) (s_rotor3 config)) 0)))
    (letrec ((aux1 
              (lambda (i mostrador message_recorder show)
                (if (= i (string-length message))
                    (if (equal? show 'y)
                        (if (equal? setting 'c) ;mostrador de codigo (des)codificado
                            (ini_code message_recorder 'd)
                            (ini_code message_recorder 'c))
                        message_recorder)
                    (if (boolean? (member #f (map (lambda(x) (if (or (and (<= x 90) (>= x 65)) (and (<= x 122) (>= x 97)) (= x 32)) #t #f)) (map (lambda(x) (char->integer x)) (string->list message)))))
                        (a_mostrador i mostrador (aux2 i mostrador message_recorder (if (and (> (char->integer (string-ref message i)) 96) (< (char->integer (string-ref message i)) 123)) #t #f)) show) 
                        #f)))) 
             (aux2            ;parte de cod/desc efetiva -> caminho da mensagem: letter -> plugboard -> 3routers -> reflector -> 3routers -> plugboard -> letter
              (lambda (i mostrador message_recorder min) ;min é #t se o carater em analise for minusculo
                (if (equal? (string-ref message i) #\space) ;se espaço, avançar na string
                    (string-set! message_recorder i #\space)
                    (and (string-set! message_recorder i (cadr (assoc (if min (integer->char (+ -32 (char->integer (string-ref message i)))) (string-ref message i)) 
                                                                      (map (lambda (x) (string->list x)) (s_plugboard config)))))
                         (code_process (s_rotor1 config) i message_recorder)
                         (code_process (s_rotor2 config) i message_recorder)
                         (code_process (s_rotor3 config) i message_recorder)
                         (code_process (s_reflector config) i message_recorder)
                         (code_process (inv_rotor (s_rotor3 config)) i message_recorder)
                         (code_process (inv_rotor (s_rotor2 config)) i message_recorder)
                         (code_process (inv_rotor (s_rotor1 config)) i message_recorder)
                         (string-set! message_recorder i (if min 
                                                             (integer->char (+ 32 (char->integer (cadr (assoc (string-ref message_recorder i) (map (lambda (x) (string->list x)) (s_plugboard config)))))))
                                                             (cadr (assoc (string-ref message_recorder i) (map (lambda (x) (string->list x)) (s_plugboard config))))))))
                (a_rotors! config mostrador posici)
                message_recorder))
             (a_mostrador ;atualiza o mostrador
              (lambda (i mostrador message_recorder show)
                (cond ((and (= (list-ref mostrador 0) 26) 
                            (= (list-ref mostrador 1) 26) 
                            (= (list-ref mostrador 2) 26)) ;(26 26 26)
                                    (aux1 (add1 i) (list 1 1 1) message_recorder show))
                      ((and (= (list-ref mostrador 0) 26) 
                            (= (list-ref mostrador 1) 26)) ;(26 26 #)
                       (aux1 (add1 i) (list 1 1 (add1 (caddr mostrador))) message_recorder show))
                      ((= (list-ref mostrador 0) 26) ;(26 # #)
                       (aux1 (add1 i) (list 1 (add1 (cadr mostrador)) (caddr mostrador)) message_recorder show))
                      (else (aux1 (add1 i) (list (add1 (car mostrador)) (cadr mostrador) (caddr mostrador)) message_recorder show)))))
             (code_process
              (lambda (compon i message_recorder)
                (string-set! message_recorder i (cadr (assoc (string-ref message_recorder i) (map (lambda (x) (string->list x)) compon)))))))
      (aux1 0 (list 2 1 1) (string-copy (make-string (string-length message) #\?)) show))))

;Jogo-tipo
(define jogo_aux1
  (lambda (message resposta codigo)
    (display resposta)
    (newline)
    (letrec ((jogada
              (lambda (i carater c lost)
                (if (equal? carater (string-copy "f_break"))
                    (display "Jogo Terminado Forçadamente.")
                    (if (or (not (string? carater)) 
                            (not (= (string-length carater) 1)) 
                            (map (lambda(x) (if (or (and (<= x 90) (>= x 65)) (and (<= x 122) (>= x 97)) (= x 32)) #t #f)) 
                                 (map (lambda(x) (char->integer x)) (string->list carater))))
                        (and (display "***Erro: não introduziste um carater!***") (newline) 
                             (jogada 0 (read) c lost))
                        (if (not (boolean? (member (string-ref carater 0) (string->list codigo))))
                            (if (= i (length (string->list message)))
                                (and (display resposta)
                                     (newline)
                                     (contin (add1 c) lost))
                                (if (equal? (string-ref carater 0) (string-ref codigo i))
                                    (and (vector-set! resposta i (string-ref carater 0))
                                         (jogada (add1 i) carater c lost))
                                    (jogada (add1 i) carater c lost)))
                            (and (display resposta)
                                 (newline)
                                 (contin (add1 c) (add1 lost))))))))
             (contin
              (lambda (c lost)
                (if (>= lost 10)
                    (and (display "Perdeste!") (newline) (display "Tentar novamente? (y se sim)") 
                         (try_again ((read))))
                    (if (member "_" (vector->list resposta))
                        (and (display "Introduz o carater a verificar") (newline) (jogada 0 (read) c lost))
                        (and (display "Parabens, adivinhaste a mensagem em ")
                             (display c)
                             (display " tentativas!"))))))
             (try_again 
              (lambda (resp)
                (if (or (equal? (string-ref resp 0) #\y) (equal? (string-ref resp 0) #\Y))
                    (jogo_aux1 message (make-vector (length (string->list message)) "_") codigo)
                    (inicia_jogo)))))
             (contin 1 0))))

(define inicia_jogo
  (lambda ()
    (and (display "Introduz a mensagem:") (newline))
    (letrec ((jogo_aux0
              (lambda (message)
                (if (equal? message (string-copy "f_break"))
                    (display "Jogo Terminado Forçadamente.")
                    (if (boolean? (code r_config_creator message 'c 'n))
                        (and (display "***Erro na introdução da mensagem!***") (newline) 
                             (inicia_jogo)) 
                        (jogo_aux1 message (make-vector (length (string->list message)) "_") (code r_config_creator message 'c 'n))))))) 
             (jogo_aux0 (read)))))