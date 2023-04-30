FROM nginx:1.23.0-alpine
WORKDIR /usr/share/nginx/html

# Copy nginx config
COPY ./configs/nginx.conf /etc/nginx/conf.d/default.conf

# Copy bundle and assets to the Nginx container
COPY ./dist /usr/share/nginx/html

EXPOSE 80