meHead::[a]->a
meHead xs=case xs of [] ->error "cannot do this!"
                    (x:_)->x

