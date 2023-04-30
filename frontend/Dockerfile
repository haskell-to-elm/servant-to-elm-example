FROM nginx:1.23.0-alpine
WORKDIR /usr/share/nginx/html

# Copy bundle and assets to the Nginx container
COPY build/index.html .
COPY build/assets ./assets
COPY build/assets-root .

# Copy nginx config
COPY ./configs/nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80