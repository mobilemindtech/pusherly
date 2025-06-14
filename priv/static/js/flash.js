window.CustomFlash = {
        container: null,
        maxItems: 5,
        
        init: function() {
            this.container = document.getElementById('flash_container') || 
                           document.querySelector('.flash-container');
            if (this.container) {
                this.maxItems = parseInt(this.container.dataset.maxItems) || 5;
            }
        },
        
        show: function(id, position, duration, closable) {
            console.log("show flash", id)
            if (!this.container) this.init();
            
            const element = document.querySelector(id);
            if (!element) {
                console.log("element not found")
                return;
            }
            
            // Limitar número de notificações
            this.enforceMaxItems();
            
            // Mostrar com animação
            element.style.display = 'block';
            setTimeout(() => {
                element.classList.add('flash-show');
            }, 10);
            
            // Auto-close se duration > 0
            if (duration > 0) {
                setTimeout(() => {
                    this.close(element);
                }, duration);
            }
            
            // Adicionar event listeners
            this.setupEventListeners(element);
        },
        
        close: function(element) {
            
            element.classList.add('flash-hide');
            setTimeout(() => {
                if (element.parentNode) {
                    element.parentNode.removeChild(element);
                }
            }, 300);
        },
        
        clearAll: function() {
            const messages = document.querySelectorAll('.custom-flash-message');
            messages.forEach(msg => {
                this.close(msg);
            });
        },
        
        enforceMaxItems: function() {
            if (!this.container) return;
            
            const messages = this.container.querySelectorAll('.custom-flash-message');
            if (messages.length >= this.maxItems) {
                // Remove as mais antigas
                for (let i = 0; i < messages.length - this.maxItems + 1; i++) {
                    this.close(messages[i]);
                }
            }
        },
        
        setupEventListeners: function(element) {
            // Pausar progresso no hover
            element.addEventListener('mouseenter', function() {
                const progress = element.querySelector('.flash-progress');
                if (progress) {
                    progress.style.animationPlayState = 'paused';
                }
            });
            
            element.addEventListener('mouseleave', function() {
                const progress = element.querySelector('.flash-progress');
                if (progress) {
                    progress.style.animationPlayState = 'running';
                }
            });
            
            // Click to close (se não tiver botão específico)
            const closeBtn = element.querySelector('.flash-close-btn');
            if (closeBtn) {
                element.addEventListener('click', () => {
                    this.close(element);
                });
            } 
        }
    };
    
    // Inicializar quando DOM estiver pronto
    document.addEventListener('DOMContentLoaded', function() {
        CustomFlash.init();
    });